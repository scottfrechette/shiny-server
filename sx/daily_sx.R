oldw <- getOption("warn")
options(warn = -1)

# Load packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(fvoa))

# Identify current week
today_week <- today() %>%
  floor_date(unit = "week", week_start = 2) %>%
  week()
start_week <- 35
current_week <- today_week - start_week
weeks_played <- current_week - 1

# Scrape data
sx_schedule <- scrape_schedule("espn", 299999)
sx_team <- scrape_team(weeks_played, "espn", 299999) %>% 
  unnest()

# Replace team names
source(here::here("sx", "sx_lookupIDs.R"))
team_ids <- espn_teamIDs(299999) %>% 
  left_join(sx_lookup_ids, by = "team_id") %>% 
  mutate(team = factor(team),
         Team = str_remove(Team, " $")) %>% 
  select(-team_id)

sx_schedule <- sx_schedule %>% 
  gather_schedule() %>% 
  mutate(Team = str_remove(Team, " $")) %>% 
  left_join(team_ids, by = "Team") %>% 
  select(-Team) %>% 
  rename(Team = team)
sx_team <- sx_team %>% 
  left_join(sx_lookup_ids, by = "team_id") %>% 
  select(team_id, Week, Team = team, Score:Points) 

# Extract scores
sx_scores <- extract_weekly_scores(sx_team)


# Run FVOA analysis
sx_simulated_season <- simulate_season(sx_schedule, sx_scores, "sx")
playoff_leverage <- read_csv(here::here("sx", "playoff_leverage.csv"))
sx_model_eval <- evaluate_model(sx_scores, output = "shiny")
sx_fvoa_season <- calculate_fvoa_season(sx_scores)
sx_matchups_prob <- all_matchups(sx_scores, type = "prob")
sx_matchups_spread <- all_matchups(sx_scores, type = "spread")
sx_rankings <- calculate_rankings(sx_schedule, sx_scores) %>% 
  mutate(`Colley Rank` = min_rank(-`Colley Rating`))
sx_current_matchups <- current_matchups(current_week, sx_schedule,
                                         sx_scores)
sx_playoff_leverage_chart <- playoff_leverage_plot(sx_scores, sx_schedule,
                                                    playoff_leverage)
sx_lineup_eval <- evaluate_lineup(sx_team, wr = 2, dl = 0, db = 0, plot = T)

# Save data for Shiny app
save(sx_schedule, sx_team, sx_scores,
     sx_simulated_season, sx_model_eval, sx_fvoa_season,
     sx_matchups_prob, sx_matchups_spread,
     sx_rankings, sx_current_matchups,
     sx_playoff_leverage_chart, sx_lineup_eval, 
     file = here::here("sx", "sx-data.RData"))

options(warn = oldw)