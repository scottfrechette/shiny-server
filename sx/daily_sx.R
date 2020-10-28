
oldw <- getOption("warn")
options(warn = -1)

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

sx_con <- dbConnect(SQLite(), here::here("sx", "sx.sqlite"))

sx_team_tmp <- scrape_team("espn", 299999, weeks_played)

if(exists("sx_team_tmp")) {
  
  dbSendQuery(sx_con, str_glue("DELETE from teams where week == {weeks_played} and season == {current_season}"))
  
  dbWriteTable(sx_con, 
               "teams", sx_team_tmp, 
               overwrite = FALSE, append = TRUE)
  
  rm(sx_team_tmp)
  
}

sx_owners <- collect(tbl(sx_con, "owners"))
sx_schedule <- tbl(sx_con, "schedule") %>% 
  filter(season == current_season) %>% 
  collect()
sx_team <- tbl(sx_con, "teams") %>% 
  filter(season == current_season) %>% 
  collect()
sx_scores <- extract_scores(sx_team)
sx_proj <- extract_projections(sx_team)

sx_simulated_scores_tmp <- simulate_season(sx_team, 1000, max(sx_schedule$week))

if(exists("sx_simulated_scores_tmp")) {
  
  dbSendQuery(sx_con, str_glue("DELETE from simulated_scores where week == {weeks_played}"))
  
  dbWriteTable(sx_con, 
               "simulated_scores", sx_simulated_scores_tmp, 
               overwrite = FALSE, append = TRUE)
  
}

sx_simulated_season_tmp <- simulate_record(sx_simulated_scores_tmp,
                                           sx_schedule,
                                           max(sx_team$week)) %>% 
  simulate_ranking(max(sx_team$week))

if(exists("sx_simulated_season_tmp")) {
  
  dbSendQuery(sx_con, str_glue("DELETE from simulated_seasons where week == {weeks_played} and season == {current_season}"))
  
  dbWriteTable(sx_con, 
               "simulated_seasons", sx_simulated_season_tmp, 
               overwrite = FALSE, append = TRUE)
  
  rm(sx_simulated_scores_tmp, sx_simulated_season_tmp)
  
}

sx_model_eval_tmp <- evaluate_model(sx_team, weeks_played, .reps = 1e6)

if(exists("sx_model_eval_tmp")) {
  
  dbSendQuery(sx_con, str_glue("DELETE from model_evaluation where week == {weeks_played} and season == {current_season}"))
  
  dbWriteTable(sx_con, 
               "model_evaluation", sx_model_eval_tmp, 
               overwrite = FALSE, append = TRUE)
  
  rm(sx_model_eval_tmp)
  
}

sx_players_tmp <- scrape_player_projections("espn", 299999, week = weeks_played, season = 2020)

if(exists("sx_players_tmp")) {
  
  dbSendQuery(sx_con, str_glue("DELETE from players where week == {weeks_played} and season == {current_season}"))
  
  dbWriteTable(sx_con, 
               "players", sx_players_tmp, 
               overwrite = FALSE, append = TRUE)
  
  rm(sx_players_tmp)
  
}

sx_schedule <- sx_schedule %>% 
  fvoa:::gather_schedule() %>% 
  mutate(teamID = as.integer(teamID)) %>% 
  left_join(sx_owners, 
            by = c("league", "leagueID", "teamID")) %>% 
  select(-teamID) %>% 
  fvoa:::spread_schedule()
sx_team <- sx_team %>% 
  left_join(sx_owners, 
            by = c("league", "leagueID", "teamID")) %>% 
  select(league:week, team, score:act_pts)
sx_scores <- extract_scores(sx_team)
sx_proj <- extract_projections(sx_team)
sx_model_eval <- collect(tbl(sx_con, "model_evaluation")) %>% 
  left_join(sx_owners, 
            by = c("league", "leagueID", "teamID")) %>% 
  select(league:week, team, opp:sim)
sx_simulated_season <- collect(tbl(sx_con, "simulated_seasons")) %>% 
  left_join(sx_owners, 
            by = c("league", "leagueID", "teamID")) %>% 
  select(league:week, team, points:rank)
sx_simulated_scores <- collect(tbl(sx_con, "simulated_scores")) %>% 
  filter(week == max(week)) %>% 
  left_join(sx_owners, 
            by = c("league", "leagueID", "teamID")) %>% 
  select(league:sim_week, team, score)

sx_playoff_leverage <- simulate_record(sx_simulated_scores, sx_schedule, weeks_played)
sx_fvoa_season <- calculate_fvoa_season(sx_scores)
sx_matchups_prob <- compare_league(sx_scores, .reps = 1e6) %>%
  fvoa:::spread_league(.output = "wp")
sx_matchups_spread <- compare_league(sx_scores, .reps = 1e6) %>% 
  fvoa:::spread_league(.output = "spread")
sx_rankings <- calculate_rankings(sx_schedule, sx_scores, "espn") %>% 
  rename_all(snakecase::to_title_case) %>% 
  rename(`ESPN Rank` = `Espn Rank`,
         FVOA = Fvoa, 
         `FVOA Rank` = `Fvoa Rank`, 
         SoS = Sos, 
         `SoS Rank` = `Sos Rank`)
sx_current_matchups <- compare_current_matchups(sx_scores, 
                                                sx_schedule, 
                                                current_week, 
                                                .reps = 1e6)
sx_playoff_leverage_chart <- plot_playoff_leverage(sx_scores, 
                                                   sx_schedule, 
                                                   sx_playoff_leverage)
sx_lineup_eval <- evaluate_lineup(sx_team, 
                                  wr = 2, 
                                  dl = 0, db = 0,
                                  plot = TRUE)

save(sx_schedule, sx_team, sx_proj, sx_scores,
     sx_simulated_season, sx_model_eval, 
     sx_fvoa_season,
     sx_matchups_prob, sx_matchups_spread,
     sx_rankings, sx_current_matchups,
     sx_playoff_leverage_chart, sx_lineup_eval, 
     file = here::here("sx", "sx-data.RData"))

