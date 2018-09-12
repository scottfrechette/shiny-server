# Load packages
library(tidyverse)
library(lubridate)
library(fvoa)
library(rvest)

# Identify current week
today_week <- today() %>%
  floor_date(unit = "week", week_start = 2) %>%
  week()
start_week <- as_date("2018-09-03") %>%
  floor_date(unit = "week", week_start = 2) %>%
  week()
current_week <- today_week - start_week
weeks_played <- current_week - 1

# Scrape data
clt_schedule <- scrape_schedule("yahoo", 150019) # run weekly for changes in team names
clt_team <- scrape_team(weeks_played, "yahoo", 150019) %>% 
  unnest()
clt_proj <- extract_weekly_scores(clt_team)
clt_scores <- clt_proj %>% select(-Proj)

# Run FVOA analysis
clt_simulated_season <- simulate_season(clt_schedule, clt_scores)
if(weeks_played > 1) {
  clt_model_eval <- evaluate_model(clt_scores)
  } else {clt_model_eval <- NULL}
clt_fvoa_season <- calculate_fvoa_season(clt_scores)
clt_matchups_prob <- all_matchups(clt_scores, type = "prob")
clt_matchups_spread <- all_matchups(clt_scores, type = "spread")
clt_rankings <- calculate_rankings(clt_schedule, clt_scores)
clt_current_matchups <- current_matchups(clt_schedule, clt_scores, current_week)

# clt_schedule %>% 
#   write_csv(here::here("clt", "clt_schedule.csv"))

save(clt_schedule, clt_proj, clt_scores,
     clt_simulated_season, clt_model_eval, clt_fvoa_season,
     clt_matchups_prob, clt_matchups_spread,
     clt_rankings, clt_current_matchups,
     file = "clt-data.RData")
