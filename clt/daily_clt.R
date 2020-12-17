
oldw <- getOption("warn")
options(warn = -1)


# Setup -------------------------------------------------------------------

# Load packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(fvoa))
suppressPackageStartupMessages(library(RSQLite))
suppressPackageStartupMessages(library(DBI))

# Identify current week
today_week <- today() %>%
  floor_date(unit = "week", week_start = 2) %>%
  week()
start_week <- 35
current_week <- today_week - start_week
weeks_played <- current_week - 1
current_season <- year(today())


# Scrape Data -------------------------------------------------------------

clt_con <- dbConnect(SQLite(), here::here("clt", "clt.sqlite"))

clt_team_tmp <- scrape_team("yahoo", 479084, weeks_played)

if(exists("clt_team_tmp")) {
  
  dbSendQuery(clt_con, str_glue("DELETE from teams where week == {weeks_played} and season == {current_season}"))
  
  dbWriteTable(clt_con, 
               "teams", clt_team_tmp, 
               overwrite = FALSE, append = TRUE)
  
  rm(clt_team_tmp)
  
}

Sys.sleep(120)

clt_wp_tmp <- scrape_win_prob(479084, current_week, 2020, "yahoo")

if(exists("clt_wp_tmp")) {
  
  dbSendQuery(clt_con, str_glue("DELETE from wp where week == {weeks_played}"))# and season == {current_season}"))
  
  dbWriteTable(clt_con, 
               "wp", clt_wp_tmp, 
               overwrite = FALSE, append = TRUE)
  
  rm(clt_wp_tmp)
  
}

Sys.sleep(120)

clt_players_tmp <- scrape_player_projections("yahoo", 479084, week = weeks_played, season = 2020)

if(exists("clt_players_tmp")) {
  
  dbSendQuery(clt_con, str_glue("DELETE from players where week == {weeks_played} and season == {current_season}"))
  
  dbWriteTable(clt_con, 
               "players", clt_players_tmp, 
               overwrite = FALSE, append = TRUE)
  
  rm(clt_players_tmp)
  
}

clt_schedule <- tbl(clt_con, "schedule") %>% 
  filter(season == current_season) %>% 
  collect()
clt_team <- tbl(clt_con, "teams") %>% 
  filter(season == current_season) %>% 
  collect()
# clt_scores <- extract_scores(clt_team)


# Run Simulations ---------------------------------------------------------

clt_simulated_scores_tmp <- simulate_season_scores(clt_team, 1000, max(clt_schedule$week))

if(exists("clt_simulated_scores_tmp")) {
  
  dbSendQuery(clt_con, str_glue("DELETE from simulated_scores where week == {weeks_played}"))
  
  dbWriteTable(clt_con, 
               "simulated_scores", clt_simulated_scores_tmp, 
               overwrite = FALSE, append = TRUE)
  
}

clt_simulated_record_tmp <- simulate_season_standings(clt_simulated_scores_tmp,
                                                      clt_schedule) %>%
  simulate_final_standings()

if(exists("clt_simulated_record_tmp")) {
  
  dbSendQuery(clt_con, str_glue("DELETE from simulated_records where week == {weeks_played} and season == {current_season}"))
  
  dbWriteTable(clt_con,
               "simulated_records", clt_simulated_record_tmp,
               overwrite = FALSE, append = TRUE)
  
  rm(clt_simulated_scores_tmp, clt_simulated_record_tmp)
  
}

clt_model_eval_tmp <- evaluate_model(clt_team, weeks_played, .reps = 1e6)

if(exists("clt_model_eval_tmp")) {
  
  dbSendQuery(clt_con, str_glue("DELETE from model_evaluation where week == {weeks_played} and season == {current_season}"))
  
  dbWriteTable(clt_con, 
               "model_evaluation", clt_model_eval_tmp, 
               overwrite = FALSE, append = TRUE)
  
  rm(clt_model_eval_tmp)
  
}


# Collect Results ---------------------------------------------------------

clt_owners <- collect(tbl(clt_con, "owners"))
clt_schedule <- clt_schedule %>% 
  fvoa:::gather_schedule() %>% 
  mutate(teamID = as.integer(teamID)) %>% 
  left_join(clt_owners, 
            by = c("league", "leagueID", "teamID")) %>% 
  select(-teamID) %>% 
  fvoa:::spread_schedule()
clt_team <- clt_team %>% 
  left_join(clt_owners, 
            by = c("league", "leagueID", "teamID")) %>% 
  select(league:week, team, score:act_pts)
clt_scores <- extract_scores(clt_team)
clt_proj <- extract_projections(clt_team)
clt_model_eval <- collect(tbl(clt_con, "model_evaluation")) %>% 
  left_join(clt_owners, 
            by = c("league", "leagueID", "teamID")) %>% 
  select(league:week, team, opp:sim)
clt_simulated_records <- collect(tbl(clt_con, "simulated_records")) %>%
  left_join(clt_owners,
            by = c("league", "leagueID", "teamID")) %>%
  select(league:week, team, pf:rank)
clt_simulated_scores <- collect(tbl(clt_con, "simulated_scores")) %>% 
  filter(week == max(week)) %>% 
  left_join(clt_owners, 
            by = c("league", "leagueID", "teamID")) %>% 
  select(league:sim_week, team, score)
clt_wp <- tbl(clt_con, "wp") %>% 
  filter(week == current_week) %>% 
  collect() %>% 
  left_join(clt_owners, 
            by = c("league", "leagueID", "teamID")) %>% 
  select(team, wp) %>% 
  mutate(wp = scales::percent(wp, accuracy = 1))


# Run Calculations --------------------------------------------------------

clt_simulated_seasons <- simulate_season_standings(clt_simulated_scores, clt_schedule)
clt_fvoa_season <- calculate_fvoa_season(clt_scores)
clt_matchups_prob <- compare_league(clt_scores, .reps = 1e6) %>%
  fvoa:::spread_league(.output = "wp")
clt_matchups_spread <- compare_league(clt_scores, .reps = 1e6) %>% 
  fvoa:::spread_league(.output = "spread")
clt_rankings <- calculate_rankings(clt_schedule, clt_scores, "yahoo") %>% 
  rename_all(snakecase::to_title_case) %>% 
  rename(FVOA = Fvoa, 
         `FVOA Rank` = `Fvoa Rank`, 
         SoS = Sos, 
         `SoS Rank` = `Sos Rank`)
clt_current_matchups <- compare_current_matchups(clt_scores, 
                                                 clt_schedule, 
                                                 current_week, 
                                                 win_prob = clt_wp,
                                                 leverage = clt_simulated_seasons,
                                                 .reps = 1e6)
clt_playoff_leverage_chart <- plot_playoff_leverage(clt_simulated_seasons)
clt_lineup_eval <- evaluate_lineup(clt_team, 
                                   flex = 0,
                                   plot = TRUE)

# Save Data ---------------------------------------------------------------

save(clt_schedule, clt_team, clt_proj, clt_scores,
     clt_simulated_records, clt_model_eval, 
     clt_fvoa_season,
     clt_matchups_prob, clt_matchups_spread,
     clt_rankings, clt_current_matchups,
     clt_playoff_leverage_chart, clt_lineup_eval, 
     file = here::here("clt", "clt-data.RData"))
