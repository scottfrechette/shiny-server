
# Setup -------------------------------------------------------------------

# Load packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(fvoa))

# Identify current week
today_week <- today() %>%
  floor_date(unit = "week", week_start = 2) %>%
  week()
start_week <- 35
current_week <- today_week - start_week
weeks_played <- current_week - 1
current_season <- year(today())

# Load Data ---------------------------------------------------------------

source(here::here("sx", "sx_owners.R"))

# Scrape Data -------------------------------------------------------------

sx_schedule_tmp <- scrape_schedule('espn')

sx_team_tmp <- map_df(1:weeks_played, scrape_team)

sx_schedule <- sx_schedule_tmp %>% 
  left_join(sx_owners, by = "teamID") %>% 
  left_join(select(sx_owners, opponent = 1, opponentID = 2), by = "opponentID") %>% 
  select(week, team, opponent)

sx_team <-sx_team_tmp %>% 
  left_join(sx_owners, by = "teamID") %>% 
  select(week, team, score:points)

sx_scores <- extract_scores(sx_team)

# Fit Model ---------------------------------------------------------------

sx_fit_season <- fit_team_season(sx_scores)
sx_fit <- slice_tail(sx_fit_season, n = 1)$model[[1]]

# Run Simulations ---------------------------------------------------------

sx_simulated_scores <- simulate_season_scores(sx_schedule, sx_fit)
sx_simulated_standings <- simulate_season_standings(sx_simulated_scores)
sx_simulated_final_standings <- simulate_final_standings(sx_simulated_standings)
sx_simulated_records <- simulate_final_standings_season(sx_fit_season, sx_schedule)

# Run Calculations --------------------------------------------------------

sx_current_matchups <- compare_current_matchups(sx_schedule, sx_fit)
sx_fvoa_season <- calculate_fvoa_season(sx_fit_season)
sx_lineup_eval <- sx_team %>% 
  evaluate_lineup(wr = 2,
                  dl = 0,
                  db = 0) %>% 
  plot_roster_skills()
sx_model_eval <- evaluate_model(sx_fit_season)
sx_schedule_luck <- plot_schedule_luck(sx_schedule, sx_scores, sx_owners, sims = 1000)

# Save Data ---------------------------------------------------------------

save(sx_schedule, 
     sx_team,
     sx_fit, 
     sx_simulated_records, 
     sx_fvoa_season,
     sx_model_eval, 
     sx_current_matchups,
     sx_lineup_eval, 
     sx_schedule_luck,
     file = here::here("sx", "sx-data.RData"))
