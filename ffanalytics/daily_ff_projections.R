
# Setup -------------------------------------------------------------------

library(ffanalytics)
library(tidyverse)
library(lubridate)
library(fvoa)

today_week <- Sys.Date() %>%
  floor_date(unit = "week", week_start = 2) %>%
  week()
start_week <- 35
current_week <- today_week - start_week

# Old Player Data ---------------------------------------------------------

library(jsonlite)
library(rvest)

scrape_yahoo_players <- function(leagueID, week, position, page) {
  
  url <- str_glue("https://football.fantasysports.yahoo.com/f1/{leagueID}/players?status=ALL&pos={position}&cut_type=9&stat1=S_PW_{week}&myteam=0&sort=AR&sdir=1&count={page}")
  
  page <- read_html(url)
  
  if(position %in% c("K", "DEF", "DL", "DB")) {
    
    tibble(player = page %>%
             html_nodes("table") %>%
             .[[2]] %>%
             html_nodes(xpath = '//*[@class="Nowrap name F-link"]') %>%
             html_text(),
           playerID = page %>%
             html_nodes("table") %>%
             .[[2]] %>%
             html_nodes(xpath = '//*[@class="Nowrap name F-link"]') %>%
             html_attr("href") %>%
             str_extract("\\d*$"),
           teamID = page %>%
             html_nodes("table") %>%
             .[[2]] %>%
             html_nodes(xpath = '//*[@class="Ta-start Nowrap Bdrend"]') %>%
             html_text() %>%
             .[-1])
    
  } else {
    
    tibble(player = page %>%
             html_nodes("table") %>%
             .[[2]] %>%
             html_nodes(xpath = '//*[@class="Nowrap name F-link"]') %>%
             html_text(),
           playerID = page %>%
             html_nodes("table") %>%
             .[[2]] %>%
             html_nodes(xpath = '//*[@class="Nowrap name F-link"]') %>%
             html_attr("href") %>%
             str_extract("\\d*$"),
           teamID = page %>%
             html_nodes("table") %>%
             .[[2]] %>%
             html_nodes(xpath = '//*[@class="Alt Ta-start Nowrap Bdrend"]') %>%
             # html_nodes(xpath = '//*[@class="Ta-start Nowrap Bdrend"]') %>%
             html_text() %>%
             .[-1])
    
  }
  
}

# clt_player_data <- crossing(position = c("QB", "RB", "WR", "TE",
#                                          "K", "DEF", "DB", "DL"),
#                             page = seq(0, 75, by = 25)) %>%
#   mutate(data = map2(position, page,
#                      ~ scrape_yahoo_players(479084, current_week, .x, .y))) %>%
#   unnest(data) %>%
#   select(-page) %>%
#   mutate(player = str_remove_all(player, " II$| III$| V$| IV$| Jr\\.$| I$|\\."),
#          teamID = if_else(str_detect(teamID, "^[A-Z] [()]"), "FA", teamID)) %>%
#   inner_join(player_table %>%
#                unite(player, first_name, last_name, sep = " ") %>%
#                mutate(player = str_remove_all(player, " II$| III$| V$| IV$| Jr\\.$| I$|\\.")) %>%
#                select(id, player),
#              by = "player")

sx_espn_data <- scrape_player_projections('espn', 299999, current_week, 2020)

# sx_player_data <- bind_rows(
#   sx_espn_data %>%
#     filter(position != "DST") %>%
#     mutate(player = str_remove_all(player, " II$| III$| V$| IV$| Jr\\.$| I$|\\.")) %>%
#     inner_join(player_table %>%
#                  unite(player, first_name, last_name, sep = " ") %>%
#                  mutate(player = str_remove_all(player, " II$| III$| V$| IV$| Jr\\.$| I$|\\.")) %>%
#                  select(id, player),
#                by = "player"),
#   sx_espn_data %>%
#     filter(position == "DST") %>%
#     mutate(player = str_remove(player, " D/ST")) %>%
#     inner_join(player_table %>%
#                  filter(position == "DST") %>%
#                  # mutate(player = str_extract(name, "\\w*$")) %>%
#                  select(id, player = last_name),
#                by = "player")
# )

# Player Data -------------------------------------------------------------

clt_player_data <- scrape_player_projections('yahoo', 479084, current_week, 2020)

sx_player_data <- scrape_player_projections('espn', 299999, current_week, 2020)

# Scoring Rules -----------------------------------------------------------

clt_baseline <- c(QB = 16, RB = 48, WR = 48, TE = 13, K = 10, DST = 10)

clt_scoring <- list(
  pass = list(
    pass_att = 0, pass_comp = 0, pass_inc = 0, pass_yds = 0.0333, pass_tds = 6,
    pass_int = -2, pass_40_yds = 0,  pass_300_yds = 0, pass_350_yds = 0,
    pass_400_yds = 0),
  rush = list(
    all_pos = TRUE,
    rush_yds = 0.1,  rush_att = 0, rush_40_yds = 0, rush_tds = 6,
    rush_100_yds = 0, rush_150_yds = 0, rush_200_yds = 0),
  rec = list(
    all_pos = TRUE,
    rec = 0, rec_yds = 0.1, rec_tds = 6, rec_40_yds = 0, rec_100_yds = 0,
    rec_150_yds = 0, rec_200_yds = 0),
  misc = list(
    all_pos = TRUE,
    fumbles_lost = -2, fumbles_total = 0,
    sacks = 0, two_pts = 2),
  kick = list(
    xp = 1.0, fg_0019 = 3.0,  fg_2029 = 3.0, fg_3039 = 3.0, fg_4049 = 3.0,
    fg_50 = 4.0,  fg_miss = 0.0),
  ret = list(
    all_pos = TRUE,
    return_tds = 6, return_yds = 0),
  idp = list(
    all_pos = TRUE,
    idp_solo = 0.5, idp_asst = 0, idp_sack = 3, idp_int = 3,  idp_fum_force = 2,
    idp_fum_rec = 2,  idp_pd = 1, idp_td = 6,  idp_safety = 2),
  dst = list(
    dst_fum_rec = 2,  dst_int = 2, dst_safety = 2, dst_sacks = 1, dst_td = 6,
    dst_blk = 1.5, dst_ret_yds = 0, dst_pts_allowed = 0),
  pts_bracket = list(
    list(threshold = 0, points = 10),
    list(threshold = 6, points = 7),
    list(threshold = 13, points = 4),
    list(threshold = 20, points = 1),
    list(threshold = 27, points = 0),
    list(threshold = 34, points = -1),
    list(threshold = 99, points = -4))
)

sx_baseline <- c(QB = 16, RB = 48, WR = 48, TE = 13, K = 10, DST = 10)

sx_scoring <- list(
  pass = list(
    pass_att = 0, pass_comp = 0, pass_inc = 0, pass_yds = 0.04, pass_tds = 4,
    pass_int = -2, pass_40_yds = 0,  pass_300_yds = 0, pass_350_yds = 0,
    pass_400_yds = 0),
  rush = list(
    all_pos = TRUE,
    rush_yds = 0.1,  rush_att = 0, rush_40_yds = 0, rush_tds = 6,
    rush_100_yds = 0, rush_150_yds = 0, rush_200_yds = 0),
  rec = list(
    all_pos = TRUE,
    rec = 0.5, rec_yds = 0.1, rec_tds = 6, rec_40_yds = 0, rec_100_yds = 0,
    rec_150_yds = 0, rec_200_yds = 0),
  misc = list(
    all_pos = TRUE,
    fumbles_lost = -2, fumbles_total = 0,
    sacks = 0, two_pts = 2),
  kick = list(
    xp = 1.0, fg_0019 = 3.0,  fg_2029 = 3.0, fg_3039 = 3.0, fg_4049 = 4.0,
    fg_50 = 5.0,  fg_miss = -1.0),
  ret = list(
    all_pos = TRUE,
    return_tds = 6, return_yds = 0),
  idp = list(
    all_pos = TRUE,
    idp_solo = 0.5, idp_asst = 0, idp_sack = 3, idp_int = 3,  idp_fum_force = 2,
    idp_fum_rec = 2,  idp_pd = 1, idp_td = 6,  idp_safety = 2),
  dst = list(
    dst_fum_rec = 2,  dst_int = 2, dst_safety = 2, dst_sacks = 1, dst_td = 6,
    dst_blk = 2, dst_ret_yds = 0, dst_pts_allowed = 0),
  pts_bracket = list(
    list(threshold = 0, points = 10),
    list(threshold = 6, points = 7),
    list(threshold = 13, points = 4),
    list(threshold = 17, points = 1),
    list(threshold = 27, points = -1),
    list(threshold = 34, points = -4),
    list(threshold = 45, points = -7),
    list(threshold = 99, points = -10))
)


# Scrape Projections -------------------------------------------------------------

my_scrape <- scrape_data(src = c("CBS", 
                                 "FantasyPros",
                                 "FantasySharks",
                                 "FFToday",
                                 "FleaFlicker",
                                 "NumberFire",
                                 "FantasyFootballNerd",
                                 "ESPN", 
                                 "FantasyData", 
                                 "Yahoo",
                                 "NFL", 
                                 "RTSports", 
                                 "Walterfootball"),
                         pos = c("QB", "RB", "WR", "TE", 
                                 "K", "DST", "DL", "DB"),
                         season = 2020, 
                         week = current_week)

# projection_sources$Yahoo$open_session(2020, 1, "QB")$scrape()

# Projections -------------------------------------------------------------

clt_projections <-  my_scrape %>% 
  projections_table(scoring_rules = clt_scoring) %>%
  # add_ecr() %>%
  # add_risk() %>%
  # add_adp() %>%
  add_player_info()

sx_projections <-  my_scrape %>% 
  projections_table(scoring_rules = sx_scoring) %>%
  # add_ecr() %>%
  # add_risk() %>%
  # add_adp() %>%
  add_player_info()

# Join Data ---------------------------------------------------------------

clt_projections_df <- clt_projections %>%
  left_join(clt_player_data %>%
              select(id, teamID),
            by = "id") %>% 
  mutate(position = case_when(
    position == "DEF" ~ "DST",
    position %in% c("CB", "S") ~ "DB",
    position %in% c("DT", "DE") ~ "DL",
    TRUE ~ position
  ))

sx_projections_df <- sx_projections %>% 
  left_join(sx_player_data %>% 
              select(id, teamID),
              # select(id, teamID, is_injured),
            by = "id") #%>% 
  # filter(!injuryStatus %in% c("OUT", "INJURY RESERVE", "SUSPENSION"))

# Save Data ---------------------------------------------------------------
last_updated <- now()

save(clt_projections_df, 
     sx_projections_df, 
     clt_player_data, 
     sx_player_data,
     last_updated,
     file = here::here("ffanalytics", "projection-data.RData"))
