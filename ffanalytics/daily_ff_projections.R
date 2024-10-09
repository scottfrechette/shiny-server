
library(tidyverse)
library(lubridate)
library(fvoa)

today_week <- Sys.Date() %>%
  floor_date(unit = "week", week_start = 2) %>%
  week()
start_week <- 35
wk <- today_week - start_week

get_ffpros_proj <- function(week = NULL,
                            position = c("qb", "rb", "wr", "te", "flex", "k", "dst"),
                            scoring = c("STD", "PPR", "HALF")) {
  
  position = match.arg(position)
  week = ifelse(is.null(week), "draft", week)
  scoring = match.arg(scoring)
  
  url <- stringr::str_glue("https://www.fantasypros.com/nfl/projections/{position}.php?max-yes=true&min-yes=true&week={week}&scoring={scoring}")
  
  html_out <- rvest::read_html(url)
  
  table_html <- html_out %>%
    rvest::html_table(header = F) %>%
    .[[1]]
  
  player_ids <- html_out %>%
    rvest::html_nodes(".player-label") %>%
    rvest::html_nodes(".fp-player-link") %>%
    rvest::html_attr("class") %>%
    stringr::str_extract_all("[0-9]+$") %>%
    unlist()
  
  if (position %in% c("k", "dst")) {
    
    cols <- as.character(table_html[1,]) %>%
      janitor:::make_clean_names()
    
    table_df <- table_html %>%
      dplyr::slice(-1) %>%
      purrr::set_names(cols) %>%
      mutate(pos = position, .after = 1)
    
  } else if (position == "flex") {
    
    cols <- paste(as.character(table_html[1,]),
                  as.character(table_html[2,]),
                  sep = "_") %>%
      gsub("^_|MISC_", "", .) %>%
      janitor:::make_clean_names()
    
    table_df <- table_html %>%
      dplyr::slice(-1, -2) %>%
      purrr::set_names(cols)
    
  } else {
    
    cols <- paste(as.character(table_html[1,]),
                  as.character(table_html[2,]),
                  sep = "_") %>%
      gsub("^_|MISC_", "", .) %>%
      janitor:::make_clean_names()
    
    table_df <- table_html %>%
      dplyr::slice(-1, -2) %>%
      purrr::set_names(cols) %>%
      mutate(pos = position, .after = 1)
  }
  
  rankings <- ffpros::fp_rankings(position) %>%
    select(ffprosID = 1, ecr, rank, best_rank = best, worst_rank = worst)
  
  table_df %>%
    type.convert(as.is = T) %>%
    dplyr::select(player, pos, fpts) %>%
    dplyr::mutate(playerID = player_ids, .before = 1) %>%
    tidyr::extract(fpts, c("projection", "high", "low"),
                   regex = "(\\d*\\.\\d)(\\d*\\.\\d)(\\d*\\.\\d)",
                   convert = T) %>%
    dplyr::arrange(-projection, high, low) %>%
    dplyr::transmute(proj_rank = dplyr::row_number(),
                     ffprosID = playerID,
                     player = stringr::str_remove(player, " highlow"),
                     pos = toupper(str_remove(pos, "\\d+")),
                     projection = projection,
                     low = low,
                     high = high) %>% 
    dplyr::left_join(rankings, by = "ffprosID")
}

plot_ffpros_proj <- function(ffpros_proj, n = 25) {
  
  if (n_distinct(ffpros_proj$pos) == 1) {
    
    ffpros_proj %>%
      head(n) %>%
      ggplot(aes(y = ecr, color = availability)) +
      geom_text(aes(x = projection, label = projection)) +
      scale_y_reverse(breaks = seq(from = 0, to = 80, by = 10)) +
      scale_x_continuous(breaks = seq(from = 0, to = max(ffpros_proj$high + 5), by = 5),
                         limits = c(0, max(ffpros_proj$high + 5)),
                         expand = c(0, NA)) +
      geom_text(aes(label = player, x = high), size = 3, hjust = -0.25) +
      geom_segment(aes(yend = ecr, x = high, xend = low)) +
      scale_color_manual(values = c("lightgrey", "#004F71", "red")) + 
      theme_minimal() +
      theme(panel.grid.minor.x = element_blank()) +
      labs(x = "Projected Points",
           y = "ECR",
           color = NULL)
    
  } else {
    
    ffpros_proj %>%
      head(n) %>%
      ggplot(aes(y = ecr, color = availability, shape = pos)) +
      geom_text(aes(x = projection, label = projection)) +
      scale_y_reverse(breaks = seq(from = 0, to = 80, by = 10)) +
      scale_x_continuous(breaks = seq(from = 0, to = max(ffpros_proj$high + 5, na.rm = T), by = 5),
                         limits = c(0, max(ffpros_proj$high + 5)),
                         expand = c(0, NA)) +
      geom_text(aes(label = player, x = high), size = 3, hjust = -0.25) +
      geom_segment(aes(yend = ecr, x = high, xend = low)) +
      scale_color_manual(values = c("lightgrey", "#004F71", "red")) + 
      theme_minimal() +
      theme(legend.position = c(.1, .1),
            panel.grid.minor.x = element_blank()) +
      guides(color = guide_legend(nrow = 1)) +
      labs(x = "Projected Points",
           y = "ECR",
           color = NULL)
    
  }
  
}

load("~/R_projects/shiny-server/ffanalytics/defenseIDs.rda")

player_projs <- tibble(roster = c("qb", "rb", "wr", "te", 
                                  "flex", "k", "dst")) %>% 
  mutate(proj = map(roster, ~get_ffpros_proj(week = wk, position = .x))) %>%
  unnest(proj) %>% 
  mutate(player = str_remove(player, " [A-z]*highlow$"))

clt_players <- get_players(week = wk, league = 'yahoo', season = 2024, leagueID = 95701) %>%
  mutate(availability = case_when(
    teamID == 8L                 ~ "Roster",
    is.na(teamID) | teamID == 0L ~ "Available",
    TRUE                         ~ "Taken")) %>%
  left_join(select(ffscrapr::dp_playerids(), yahooID = yahoo_id, ffprosID = fantasypros_id)) %>% 
  mutate(ffprosID = if_else(yahooID == "34207", "24333", ffprosID))

players <- distinct(left_join(player_projs, select(clt_players, ffprosID, availability), by = "ffprosID"))

players %>%
  filter(roster == "flex") %>% 
  filter(availability != "Taken") %>%
  plot_ffpros_proj(n = 15)
