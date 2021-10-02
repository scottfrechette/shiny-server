
# Setup -------------------------------------------------------------------

library(tidyverse)
library(ffanalytics)
library(lubridate)
library(fvoa)

today_week <- Sys.Date() %>%
  floor_date(unit = "week", week_start = 2) %>%
  week()
start_week <- 35
current_week <- today_week - start_week

# Scrape Projections -------------------------------------------------------------

ffa_data <- get_ffa_data(week = current_week,
                         src = c('CBS', 
                                 'ESPN',
                                 'FantasyData', 
                                 'FantasyPros', 
                                 'FantasySharks', 
                                 'FFToday', 
                                 'FleaFlicker', 
                                 'NumberFire',
                                 # 'Yahoo', 
                                 'FantasyFootballNerd', 
                                 'NFL'))

# Projections -------------------------------------------------------------

clt_projections <- calculate_ffa_projections(ffa_data, 'yahoo')
sx_projections <- calculate_ffa_projections(ffa_data, 'espn')

# Scrape Players ----------------------------------------------------------

clt_player_data <- get_players(current_week, 'yahoo')
sx_player_data <- get_players(current_week, 'espn')

# Join Data ---------------------------------------------------------------

clt_projections_df <- left_join(clt_projections,
                                select(clt_player_data, mflID, teamID),
                                by = "mdlID") %>% 
  mutate(position = case_when(
    position == "DEF" ~ "DST",
    position %in% c("CB", "S") ~ "DB",
    position %in% c("DT", "DE") ~ "DL",
    TRUE ~ position
  ))

sx_projections_df <-left_join(sx_projections,
                              select(sx_player_data, mflID, teamID),
                              by = "mflID")

# Save Data ---------------------------------------------------------------

last_updated <- now()

if(exists("clt_projections_df")) {
  
  save(sx_projections_df,
       clt_projections_df,
       last_updated,
       file = here::here("ffanalytics", "projection-data.RData"))
  
} else {
  
  save(sx_projections_df,
       last_updated,
       file = here::here("ffanalytics", "projection-data.RData"))
  
}
