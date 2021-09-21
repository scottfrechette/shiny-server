
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

sx_con <- dbConnect(SQLite(), here::here("sx", "sx.sqlite"))

sx_schedule_tmp <- scrape_schedule('espn')

sx_team_tmp <- map_df(1:weeks_played, scrape_team)

sx_players_tmp <- scrape_player_projections(current_week, 'espn')

sx_owners <- collect(tbl(sx_con, 'owners'))

sx_schedule <- sx_schedule_tmp %>% 
  left_join(sx_owners, by = "teamID") %>% 
  left_join(select(sx_owners, opponent = team, opponentID = teamID), 
            by = "opponentID") %>% 
  select(week, team, opponent)
sx_team <-sx_team_tmp %>% 
  left_join(sx_owners, by = "teamID") %>% 
  select(week, team, score:points)
sx_scores <- extract_scores(sx_team)

# Fit Model ---------------------------------------------------------------

sx_fit <- fit_model(sx_scores)

# Run Simulations ---------------------------------------------------------

sx_simulated_scores <- simulate_season_scores(sx_schedule, sx_fit)
sx_simulated_standings <- simulate_season_standings(sx_simulated_scores)
sx_simulated_final_standings_tmp <- simulate_final_standings(sx_simulated_standings) %>% 
  left_join(sx_owners, by = "team") %>%
  select(season, week, teamID, pf:rank)

if(exists("sx_simulated_final_standings_tmp")) {
  
  dbSendQuery(sx_con, str_glue("DELETE from simulated_records where week == {weeks_played} and season == {current_season}"))
  
  dbWriteTable(sx_con,
               "simulated_records", sx_simulated_final_standings_tmp,
               overwrite = FALSE, append = TRUE)
  
}


# Collect Results ---------------------------------------------------------

sx_team <- sx_team %>% 
  select(week, team, score:points)
sx_scores <- extract_scores(sx_team)
sx_proj <- extract_projections(sx_team)
sx_simulated_records <- collect(tbl(sx_con, "simulated_records")) %>%
  filter(season == current_season) %>%
  left_join(sx_owners,
            by = "teamID") %>%
  select(week, team, pf:rank)

# Run Calculations --------------------------------------------------------

sx_fvoa_season <- calculate_fvoa_season(sx_scores)
sx_matchups_prob <- compare_league(sx_fit) %>%
  fvoa:::spread_league(.output = "wp")
sx_matchups_spread <- compare_league(sx_fit) %>% 
  fvoa:::spread_league(.output = "spread")
sx_rankings <- calculate_rankings(sx_schedule, sx_fit, "espn") %>% 
  rename_all(snakecase::to_title_case) %>% 
  rename(PF = Pf,
         PA = Pa,
         WP = Wp,
         `ESPN Rank` = `Espn Rank`,
         FVOA = Fvoa, 
         `FVOA Rank` = `Fvoa Rank`, 
         SoS = Sos, 
         `SoS Rank` = `Sos Rank`)
sx_current_matchups <- compare_current_matchups(sx_schedule, sx_fit)
sx_playoff_leverage_chart <- plot_playoff_leverage(sx_simulated_standings)
sx_lineup_eval <- sx_team %>% 
  mutate(league = 'espn') %>% 
  rename(act_pts = points) %>% 
  evaluate_lineup(flex = 0,
                  plot = TRUE)
sx_model_eval <- evaluate_model(sx_scores)

# Save Data ---------------------------------------------------------------

save(sx_schedule, sx_team, sx_proj, sx_scores,
     sx_fit, sx_simulated_records, 
     sx_fvoa_season, sx_model_eval, 
     sx_matchups_prob, sx_matchups_spread,
     sx_rankings, sx_current_matchups,
     sx_playoff_leverage_chart, sx_lineup_eval, 
     file = here::here("sx", "sx-data.RData"))
