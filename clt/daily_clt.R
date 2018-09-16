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
clt_yahoo_win_prob <- scrape_win_prob(current_week, "yahoo", 150019)
playoff_leverage <- read_csv(here::here("clt", "playoff_leverage.csv"))

# Replace team names
source(here::here("clt", "lookup_id.R"))
team_ids <- yahoo_teamIDs(150019) %>% 
  left_join(lookup_id, by = "team_id") %>% 
  mutate(team = factor(team)) %>% 
  select(-team_id)

clt_schedule <- clt_schedule %>% 
  left_join(team_ids, by = "Team") %>% 
  select(-Team) %>% 
  rename(Team = team)
clt_team <- clt_team %>% 
  left_join(lookup_id, by = "team_id") %>% 
  select(team_id, Week, Team = team, Score:Points) 
clt_yahoo_win_prob <- clt_yahoo_win_prob %>%
  left_join(lookup_id, by = "team_id") %>% 
  select(team_id, Team = team, type, win_prob)

# Extract scores
clt_proj <- extract_weekly_scores(clt_team)
clt_scores <- clt_proj %>% select(-Proj)


# Run FVOA analysis
clt_simulated_season <- simulate_season(clt_schedule, clt_scores, "clt") %>% 
  left_join(team_ids, by = "Team") %>% 
  select(-Team) %>% 
  rename(Team = team) %>% 
  select(Team, everything())
if(weeks_played > 1) {
  clt_model_eval <- evaluate_model(clt_scores)
  } else {clt_model_eval <- NULL}
clt_fvoa_season <- calculate_fvoa_season(clt_scores)
clt_matchups_prob <- all_matchups(clt_scores, type = "prob")
clt_matchups_spread <- all_matchups(clt_scores, type = "spread")
clt_rankings <- calculate_rankings(clt_schedule, clt_scores)
clt_current_matchups <- current_matchups(current_week, clt_schedule, 
                                         clt_scores, clt_yahoo_win_prob)
clt_playoff_leverage_chart <- playoff_leverage_plot(clt_scores, clt_schedule, 
                                                    playoff_leverage)
clt_lineup_eval <- evaluate_lineup(clt_team, plot = T)

save(clt_schedule, clt_team, clt_proj, clt_scores,
     clt_simulated_season, clt_model_eval, clt_fvoa_season,
     clt_matchups_prob, clt_matchups_spread,
     clt_rankings, clt_current_matchups,
     clt_playoff_leverage_chart, clt_lineup_eval, 
     file = here::here("clt", "clt-data.RData"))
