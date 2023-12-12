
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

clt_team_tmp <- get_team(weeks_played, 'yahoo', leagueID = 102347) %>% 
  mutate(season = current_season, .before = 1)

if(exists("clt_team_tmp")) {
  
  dbSendQuery(clt_con, str_glue("DELETE from teams where week == {weeks_played} and season == {current_season}"))
  
  dbWriteTable(clt_con,
               "teams", clt_team_tmp,
               overwrite = FALSE, append = TRUE)
  
  rm(clt_team_tmp)
  
}

Sys.sleep(60)

clt_wp_tmp <- get_win_prob(current_week, leagueID = 102347) %>% 
  mutate(season = current_season,
         week = current_week, .before = 1)

# Collect Results ---------------------------------------------------------

clt_owners <- tbl(clt_con, 'owners') %>% 
  filter(season == current_season) %>% 
  select(teamID, team) %>% 
  collect()
clt_schedule <- tbl(clt_con, "schedule") %>% 
  filter(season == current_season) %>% 
  collect() %>% 
  left_join(clt_owners, by = "teamID") %>% 
  left_join(select(clt_owners, opponent = team, opponentID = teamID),
            by = "opponentID") %>% 
  select(season, week, team, opponent)
clt_team <- tbl(clt_con, "teams") %>% 
  filter(season == current_season) %>% 
  collect() %>% 
  left_join(clt_owners, by = "teamID") %>% 
  select(week, team, score:points)
clt_scores <- extract_scores(clt_team)
clt_proj <- extract_projections(clt_team)
clt_wp <- clt_wp_tmp %>%
  left_join(clt_owners, by = "teamID") %>% 
  select(week, team, wp) %>% 
  mutate(wp = scales::percent(wp, accuracy = 1))

# Fit Model ---------------------------------------------------------------

clt_fit_season <- fit_team_season(clt_scores)
clt_fit <- slice_tail(clt_fit_season, n = 1)$model[[1]]
clt_draws <- fvoa:::extract_team_draws(clt_fit)

# Run Simulations ---------------------------------------------------------

clt_simulated_scores <- simulate_season_scores(clt_schedule, clt_fit)
clt_simulated_standings <- simulate_season_standings(clt_simulated_scores)
clt_simulated_final_standings <- simulate_final_standings(clt_simulated_standings)
clt_simulated_records <- simulate_final_standings_season(clt_fit_season, clt_schedule)

# Run Calculations --------------------------------------------------------

clt_current_matchups <- compare_current_matchups(clt_schedule, clt_fit, clt_wp, quality = T, clt_simulated_standings) %>% 
  select(-Yahoo)
clt_fvoa_season <- calculate_fvoa_season(clt_fit_season)
clt_rankings <- calculate_rankings(clt_schedule, clt_fit) %>% 
  set_names("Team", "PF", "PA", 
            "Record", "WP", "Yahoo Rank", 
            "FVOA", "FVOA Rank",
            "SOS Favored", "SOS", "SOS Rank", 
            "SOS Played Favored", "SOS Played Avg FVOA", "SOS Played Margin", "SOS Played", "SOS Played Rank",
            "SOS Remaining Favored", "SOS Remaining", "SOS Remaining Rank",
            "SOR", "SOR Rank",
            "Colley Rating", "Colley Rank")
clt_lines <- compare_league(clt_fit) %>% 
  fvoa:::spread_league(.output = "spread") %>% 
  rename(Team = team)
clt_lineup_eval <- clt_team %>% 
  evaluate_lineup(flex = 0) %>% 
  plot_roster_skills()
clt_model_eval <- evaluate_model(clt_fit_season)
clt_playoff_leverage <- plot_playoff_leverage(clt_simulated_standings)
clt_schedule_luck <- plot_schedule_luck(clt_schedule, clt_scores, clt_owners, sims = 1000)

# Save Data ---------------------------------------------------------------

save(clt_schedule, 
     clt_team, 
     clt_draws, 
     clt_simulated_records, 
     clt_rankings,
     clt_lines,
     clt_fvoa_season,
     clt_model_eval,
     clt_current_matchups,
     clt_lineup_eval,
     clt_playoff_leverage,
     clt_schedule_luck,
     file = here::here("clt", "clt-data.RData"))
