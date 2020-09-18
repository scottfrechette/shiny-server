
# Setup -------------------------------------------------------------------

library(ffanalytics)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(rvest)

today_week <- Sys.Date() %>%
  floor_date(unit = "week", week_start = 2) %>%
  week()
start_week <- 35
current_week <- today_week - start_week

# CLT Player Data ---------------------------------------------------------

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

clt_player_data <- crossing(position = c("QB", "RB", "WR", "TE", 
                                         "K", "DEF", "DB", "DL"),
                            page = seq(0, 75, by = 25)) %>% 
  mutate(data = map2(position, page, 
                     ~ scrape_yahoo_players(479084, current_week, .x, .y))) %>% 
  unnest(data) %>% 
  select(-page) %>% 
  mutate(player = str_remove_all(player, " II$| III$| V$| IV$| Jr\\.$| I$|\\."),
         teamID = if_else(str_detect(teamID, "^[A-Z] [()]"), "FA", teamID)) %>%
  inner_join(player_table %>% 
               unite(player, first_name, last_name, sep = " ") %>% 
               mutate(player = str_remove_all(player, " II$| III$| V$| IV$| Jr\\.$| I$|\\.")) %>% 
               select(id, player),
             by = "player")

# SX Player Data ----------------------------------------------------------

scrape_espn_players <- function(week) {
  
  
  #' ESPN Fantasy Football API
  #'
  #' @param path path to pull
  #' @param query query parameters
  #' @param headers headers
  #' @keywords internal
  ffespn_api <- function(path, query = NULL, headers = NULL) {
    # url
    baseurl <- "http://fantasy.espn.com/"
    path <- paste0("apis/v3/games/ffl/", path)
    url <- httr::modify_url(baseurl, path = path, query = query)
    
    # get data
    if (is.null(headers)) {
      resp <- httr::GET(url, headers)
    } else {
      resp <- httr::GET(url, headers)
    }
    
    # get content
    page <- httr::content(resp, "text", encoding = "utf-8")
    
    # check errors
    if (httr::http_error(resp)) {
      stop(
        sprintf(
          "ESPN API request failed [%s]\n[%s]",
          httr::status_code(resp), url
        ),
        call. = FALSE
      )
    }
    
    # check data type
    if (httr::http_type(resp) != "application/json") {
      stop("API did not return html", call. = FALSE)
    }
    
    # parse content
    x <- jsonlite::fromJSON(page, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
    
    x
  }
  
  #' Projections
  #' @param season integer year
  #' @param week integer week (0 - 17)
  #' @param pos character position. Ex. "QB", "RB", "RB/WR", "DST", "FLEX", "DT", ...
  #'
  #' @export
  ffespn_projections <- function(leagueID = 299999, season = 2020, week, 
                                 pos = slot_names,
                                 projections = TRUE) {
    # validate input
    pos <- match.arg(pos)
    stopifnot(is.numeric(week), is.numeric(season), is.character(pos))
    stopifnot(is_scalar(week), is_scalar(season), is_scalar(pos))
    stopifnot(week >= 0L, week <= 17L)
    
    # convert input
    season <- as.integer(season)
    week <- as.integer(week)
    posID <- slot_name_to_id(pos)
    projID <- as.integer(projections)
    
    # build path
    path <- sprintf("seasons/%s/segments/0/leagues/%s", season, leagueID)
    
    # build query
    query <- list("view" = "kona_player_info")
    
    # build headers
    players = list(
      filterSlotIds = list(value = posID),
      filterStatsForSourceIds = list(value = projID), # 0 = actual, 1 = projected
      offset = jsonlite::unbox(0)
    )
    
    # required different filters for season vs weekly projections
    if (week == 0) {
      players$filterStatsForExternalIds = list(value = season)
    } else {
      players$filterStatsForSplitTypeIds = list(value = c(week))
    }
    
    # combine
    x_fantasy_filter <- list("players" = players)
    headers <- httr::add_headers(.headers = c(
      "X-Fantasy-Filter" = jsonlite::toJSON(x_fantasy_filter),
      "X-Fantasy-Source" = "kona",
      "Accept" =  "application/json",
      "Referer" =  "http://fantasy.espn.com/football/players/projections",
      "DNT" = "1",
      "Connection" =  "keep-alive",
      "X-Fantasy-Platform" = "kona-PROD-669a217c5628b670cf386bd4cebe972bf88022eb"
    ))
    
    
    # GET (with no headers it returns all teams?)
    x <- ffespn_api(path, query, headers)
    
    # check that data is not empty
    if (identical(length(x$players), 0L)) {
      stop("results are empty. no players found", call. = FALSE)
    }
    
    # parse json
    tidy_projections(x)
  }
  
  #' Tidy projections
  #'
  #' @param x data frame of projections
  #'
  #' @keywords internal
  tidy_projections <- function(x) {
    # convert json to data frame
    x <- tibble::as_tibble(purrr::simplify_all(purrr::transpose(x$players)))
    
    # convert column types
    x$id <- as.character(x$id)
    
    if ("waiverProcessDate" %in% names(x)) {
      x <- unnest(x, waiverProcessDate, keep_empty = T)
      
      x$waiverProcessDate <- as.POSIXct(x$waiverProcessDate/1000, origin = "1970-01-01", tz = "America/New_York")
    }
    
    # parse rating
    ratings <- purrr::map(x$ratings, tidy_projection_ratings)
    
    # parse player
    player <-  purrr::simplify_all(purrr::transpose(x$player))
    player$id <- as.character(player$id)
    
    player$team <- team_id_to_name(player$proTeamId)
    player$proTeamId <- NULL
    
    if ("lastNewsDate" %in% names(player)) {
      player$lastNewsDate <- list_to_dt(player$lastNewsDate)
    }
    
    if ("lastVideoDate" %in% names(player)) {
      player$lastVideoDate <- list_to_dt(player$lastVideoDate)
    }
    
    if ("injuryStatus" %in% names(player)) {
      player$injuryStatus <- purrr::simplify(tidyr::replace_na(player$injuryStatus, NA_character_))
    }
    
    if ("jersey" %in% names(player)) {
      player$jersey <- purrr::simplify(tidyr::replace_na(player$jersey, NA_character_))
    }
    
    if ("seasonOutlook" %in% names(player)) {
      if (!is.list(player$seasonOutlook)) {
        player$seasonOutlook <- dplyr::if_else(nchar(player$seasonOutlook) == 0, NA_character_, player$seasonOutlook)
      }
    }
    
    player$defaultPosition <- pos_id_to_name(player$defaultPositionId)
    player$defaultPositionId <- NULL
    
    player$stats <- purrr::map(player$stats, tidy_projection_stats)
    player$eligibleSlots <- purrr::simplify_all(player$eligibleSlots)
    player$eligibleSlots <- purrr::map(player$eligibleSlots, slot_id_to_name)
    
    player <- tibble::as_tibble(player)
    
    player$teamID <- x$onTeamId
    
    # parse player ownership
    ownership <- lapply(player$ownership, tidyr::replace_na, NA_real_)
    ownership <- purrr::map(ownership, function(x) {
      if (is.list(x)) {
        x$date <- list_to_dt(x$date)
        return(x)
      } else {
        return(list())
      }
      
    })
    
    ownership <- purrr::map(ownership, as_tibble_snake)
    player$ownership <- ownership
    
    player <- unnest(player, ownership, keep_empty = TRUE)
    
    # player rankings
    if ("rankings" %in% names(player)) {
      player$rankings <- purrr::map(player$rankings, tidy_projection_rankings)
    }
    
    # ranks by rank type
    player$draftRanksByRankType <- purrr::map(player$draftRanksByRankType, tidy_projection_draft_ranks)
    
    # prepare data frame
    x <- player
    x <- dplyr::select(x, "id", "fullName", "team", "defaultPosition", dplyr::everything())
    x$ratings <- ratings
    
    # clean column names
    colnames(x) <- camel_to_snake(colnames(x))
    x <- dplyr::select(
      x,
      "teamID" = "team_i_d",
      "playerID" = "id",
      "player" = "full_name",
      "position" = "default_position",
      "nfl_team" = "team",
      "is_active" = "active",
      "is_droppable" = "droppable",
      "is_injured" = "injured",
      "pct_owned" = "percent_owned",
      "pct_start" = "percent_started",
      "pct_change" = "percent_change",
      # "slots" = "eligible_slots",
      # "draft_ranks" = "draft_ranks_by_rank_type",
      "ratings",
      # "notes" = "season_outlook",
      "stats"
    )
    
    # unnest the stats (projections)
    # x <- tidyr::unnest(x, "stats", keep_empty = T)
    
    # return data frame
    x
  }
  
  tidy_projection_ratings <- function(ratings) {
    if (is.null(ratings)) return(tibble::tibble())
    
    ratings <- purrr::simplify_all(purrr::transpose(ratings))
    df <- as_tibble_snake(ratings)
    df$week <- as.integer(names(ratings[[1]]))
    df <- dplyr::select(df, "week", dplyr::everything())
    df
  }
  
  tidy_projection_stats <- function(stats) {
    if (length(stats) == 0) return(tibble::tibble())
    
    # parse applied total (fpts)
    if (!is.list(stats)) print(stats)
    fpts_proj <- stats[[1]]$appliedTotal
    
    # parse individual stats
    stats <- stats[[1]]$stats
    names(stats) <- stat_id_to_name(names(stats))
    
    # put into dataframe
    df <- as_tibble_snake(stats)
    df$fpts_proj <- fpts_proj
    
    # order columns to put unknown stats at the end?
    df <- dplyr::select(df, -dplyr::starts_with("stat_"), dplyr::starts_with("stat_"))
    #df <- dplyr::select(df, -dplyr::starts_with("stat_"))
    
    # return data frame
    return(df)
  }
  
  tidy_projection_rankings <- function(x) {
    if (length(x) == 0) return(tibble::tibble())
    x <- x[[1]]
    df <- tibble::as_tibble(purrr::simplify_all(purrr::transpose(x)))
    colnames(df) <- camel_to_snake(colnames(df))
    df
  }
  
  tidy_projection_draft_ranks <- function(x) {
    if (length(x) == 0) return(tibble::tibble())
    purrr::map_dfr(x, as_tibble_snake)
  }
  
  is_scalar <- function(x) identical(length(x), 1L)
  
  list_to_dt <- function(x) {
    x <- vapply(x, function(d) if (is.null(d)) NA_real_ else d/1000, double(1L))
    as.POSIXct(x, origin = "1970-01-01", tz = "America/New_York")
  }
  
  team_id_to_name <- function(id) {
    rows <- purrr::map_int(id, function(x) {
      matches <- x == team_ids$id
      if (any(matches)) which(matches)[1]
      else NA_integer_
    })
    
    team_ids$name[rows]
  }
  
  team_name_to_id <- function(name) {
    rows <- purrr::map_int(name, function(x) {
      matches <- x == team_ids$name
      if (any(matches)) which(matches)[1]
      else NA_integer_
    })
    
    team_ids$id[rows]
  }
  
  stat_id_to_name <- function(id) {
    dplyr::case_when(
      # passing
      id == 0 ~ "pass_att",
      id == 1 ~ "pass_cmp",
      id == 2 ~ "pass_inc",
      id == 3 ~ "pass_yds",
      id == 4 ~ "pass_tds",
      #id == 10 ~ "pass_sacked",
      id == 15 ~ "pass_tds_40_plus_yds",
      id == 16 ~ "pass_tds_50_plus_yds",
      id == 17 ~ "pass_yds_300_399",
      id == 18 ~ "pass_yds_400_plus",
      id == 19 ~ "pass_2pt",
      id == 20 ~ "pass_int",
      
      # rushing
      id == 23 ~ "rush_att",
      id == 24 ~ "rush_yds",
      id == 25 ~ "rush_tds",
      id == 26 ~ "rush_2pt",
      id == 35 ~ "rush_td_40_plus_yds",
      id == 36 ~ "rush_td_50_plus_yds",
      id == 37 ~ "rush_yds_100_199",
      id == 38 ~ "rush_yds_200_plus",
      
      # receptions
      id == 42 ~ "rec_yds",
      id == 43 ~ "rec_tds",
      id == 44 ~ "rec_2pt",
      id == 53 ~ "rec_cmp",
      id == 58 ~ "rec_tgt",
      
      # misc
      id == 64 ~ "sacked",
      id == 68 ~ "fumbles",
      id == 72 ~ "fumbles_lost",
      
      # kicking
      id == 74 ~ "fg_cmp_50",
      id == 75 ~ "fg_att_50",
      id == 76 ~ "fg_miss_50",
      id == 77 ~ "fg_cmp_40_49",
      id == 78 ~ "fg_att_40_49",
      id == 79 ~ "fg_miss_40_49",
      id == 80 ~ "fg_cmp_1_39",
      id == 81 ~ "fg_att_1_39",
      id == 82 ~ "fg_miss_1_39",
      id == 83 ~ "fg_cmp_tot",
      id == 84 ~ "fg_att_tot",
      id == 85 ~ "fg_miss_tot",
      id == 86 ~ "fg_cmp_xp",
      id == 87 ~ "fg_att_xp",
      id == 88 ~ "fg_miss_xp",
      
      # defense
      id == 89 ~ "def_pts_against_0",
      id == 90 ~ "def_pts_against_1_6",
      id == 91 ~ "def_pts_against_7_13",
      id == 92 ~ "def_pts_against_14_17",
      id == 93 ~ "def_block_ret_tds",
      id == 94 ~ "def_total_ret_tds",
      id == 95 ~ "def_ints",
      id == 96 ~ "def_fumbles_recovered",
      id == 97 ~ "def_blocks",
      id == 98 ~ "def_safeties",
      id == 99 ~ "def_sacks",
      id == 101 ~ "special_kick_ret_tds",
      id == 102 ~ "special_punt_ret_tds",
      id == 103 ~ "def_fumble_ret_tds",
      id == 104 ~ "def_int_ret_tds",
      id == 105 ~ "def_tds",
      id == 106 ~ "def_fumbles_forced",
      id == 107 ~ "def_tackles_assisted", # tackles assisted
      id == 108 ~ "def_tackles_solo",
      id == 109 ~ "def_tackles_total", # total tackles
      id == 110 ~ "def_stuffs",
      id == 113 ~ "def_passes_defended", # passes defended
      id == 120 ~ "def_pts_against",
      id == 121 ~ "def_pts_against_18_20",
      id == 122 ~ "def_pts_against_21_27",
      id == 123 ~ "def_pts_against_28_34",
      id == 124 ~ "def_pts_against_35_45",
      id == 125 ~ "def_pts_against_46_plus",
      id == 127 ~ "def_yds_against",
      
      # punts
      id == 138 ~ "punts",
      id == 139 ~ "punts_yds",
      id == 140 ~ "punts_inside_10",
      id == 141 ~ "punts_inside_20",
      id == 142 ~ "punts_blocked",
      id == 143 ~ "punts_returned",
      id == 144 ~ "punts_return_yds",
      id == 145 ~ "punts_touchbacks",
      id == 146 ~ "punts_fair_catches",
      
      # punts (need to be verified)
      id == 148 ~ "punts_44_plus",
      id == 149 ~ "punts_42_44",
      id == 150 ~ "punts_40_42",
      id == 151 ~ "punts_38_40",
      id == 152 ~ "punts_36_38",
      id == 153 ~ "punts_34_36",
      id == 154 ~ "punts_32_34",
      
      # misc
      id == 210 ~ "games",
      TRUE ~ paste0("stat_", id)
    )
  }
  
  pos_id_to_name <- function(x) {
    dplyr::case_when(
      x == 1 ~ "QB",
      x == 2 ~ "RB",
      x == 3 ~ "WR",
      x == 4 ~ "TE",
      x == 5 ~ "K",
      x == 7 ~ "P",
      x == 9 ~ "DT",
      x == 10 ~ "DE",
      x == 11 ~ "LB",
      x == 12 ~ "CB",
      x == 13 ~ "S",
      TRUE ~ paste0("pos_", x)
    )
  }
  
  slot_names <- c("QB", "TQB", "RB", "RB/WR", "WR", "WR/TE", "TE", "OP",
                  "DT", "DE", "LB", "DL", "CB", "S", "DB", "DP", "DST",
                  "K", "P", "HC", "FLEX", "EDR")
  
  slot_name_to_id <- function(x) {
    # QB: 0, RB: 2, WR: 4, TE: 6, DST: 16, K: 17
    dplyr::case_when(
      x == "QB" ~ 0L,
      x == "TQB" ~ 1L, # team quarterback
      x == "RB" ~ 2L,
      x == "RB/WR" ~ 3L,
      x == "WR" ~ 4L,
      x == "WR/TE" ~ 5L,
      x == "TE" ~ 6L,
      x == "OP" ~ 7L, # offensive player
      x == "DT" ~ 8L,
      x == "DE" ~ 9L,
      x == "LB" ~ 10L,
      x == "DL" ~ 11L,
      x == "CB" ~ 12L,
      x == "S" ~ 13L,
      x == "DB" ~ 14L,
      x == "DP" ~ 15L, # defensive player
      x == "DST" ~ 16L,
      x == "K" ~ 17L,
      x == "P" ~ 18L,
      x == "HC" ~ 19L, # head coach
      x == "FLEX" ~ 23L,
      x == "EDR" ~ 24L,
      TRUE ~ NA_integer_
    )
  }
  
  slot_id_to_name <- function(x) {
    x <- as.numeric(x)
    dplyr::case_when(
      x == 0 ~ "QB",
      x == 1 ~ "TQB", # team quarterback
      x == 2 ~ "RB",
      x == 3 ~ "RB/WR",
      x == 4 ~ "WR",
      x == 5 ~ "WR/TE",
      x == 6 ~ "TE",
      x == 7 ~ "OP", # offensive player
      x == 8 ~ "DT",
      x == 9 ~ "DE",
      x == 10 ~ "LB",
      x == 11 ~ "DL",
      x == 12 ~ "CB",
      x == 13 ~ "S",
      x == 14 ~ "DB",
      x == 15 ~ "DP", # defensive player
      x == 16 ~ "DST",
      x == 17 ~ "K",
      x == 18 ~ "P",
      x == 19 ~ "HC",
      x == 23 ~ "FLEX",
      x == 24 ~ "EDR", # edge rusher
      TRUE ~ paste0("slot_", x)
    )
  }
  
  camel_to_snake <- function(x) {
    tolower(gsub("(?<!^)(?=[A-Z])", "_", x, perl = TRUE))
  }
  
  as_tibble_snake <- function(x) {
    df <- tibble::as_tibble(x)
    colnames(df) <- camel_to_snake(colnames(df))
    df
  }
  
  team_ids <- tibble::tribble(
    ~id,~name,
    22L, "ARI",
    1L, "ATL",
    33L, "BAL",
    2L, "BUF",
    29L, "CAR",
    3L, "CHI",
    4L, "CIN",
    5L, "CLE",
    6L, "DAL",
    7L, "DEN",
    8L, "DET",
    9L, "GB",
    34L, "HOU",
    11L, "IND",
    30L, "JAX",
    12L, "KC",
    24L, "LAC",
    14L, "LA",
    15L, "MIA",
    16L, "MIN",
    17L, "NE",
    18L, "NO",
    19L, "NYG",
    20L, "NYJ",
    13L, "OAK",
    21L, "PHI",
    23L, "PIT",
    26L, "SEA",
    25L, "SF",
    27L, "TB",
    10L, "TEN",
    28L, "WAS",
    0, "FA"
  )
  
  df <- tibble(week = week,
               pos = c("QB", "RB", "WR", "TE", "DST", "K")) %>%
    mutate(data = map2(week, pos, ~ ffespn_projections(week = .x, pos = .y))) %>%
    unnest(data)

  df
  
}

sx_espn_data <- scrape_espn_players(week = current_week)

sx_player_data <- bind_rows(
  sx_espn_data %>% 
    filter(position != "DST",
           nfl_team > 0) %>% 
    mutate(player = str_remove_all(player, " II$| III$| V$| IV$| Jr\\.$| I$|\\.")) %>% 
    inner_join(player_table %>% 
                 unite(player, first_name, last_name, sep = " ") %>% 
                 mutate(player = str_remove_all(player, " II$| III$| V$| IV$| Jr\\.$| I$|\\.")) %>% 
                 select(id, player),
               by = "player"),
  sx_espn_data %>% 
    filter(position == "DST") %>%
    mutate(player = str_remove(player, " D/ST")) %>%
    inner_join(player_table %>%
                 filter(position == "DST") %>% 
                 # mutate(player = str_extract(name, "\\w*$")) %>% 
                 select(id, player = last_name), 
               by = "player")
)

# Scrape Projections -------------------------------------------------------------

my_scrape <- scrape_data(src = c("CBS", 
                                 "FantasyPros",
                                 "FantasySharks",
                                 "FFToday",
                                 "FleaFlicker",
                                 "NumberFire",
                                 "FantasyFootballNerd",
                                 "ESPN", "FantasyData", "Yahoo",
                                 "NFL", "RTSports", "Walterfootball"),
                         pos = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "DB"),
                         season = 2020, week = current_week)

# projection_sources$Yahoo$open_session(2020, 1, "QB")$scrape()

# CLT Projections ------------------------------------------------------------

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

clt_projections <-  my_scrape %>% 
  projections_table(scoring_rules = clt_scoring) %>%
  add_ecr() %>%
  add_risk() %>%
  # add_adp() %>%
  add_player_info()


# SX Projections ----------------------------------------------------------

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

sx_projections <-  projections_table(my_scrape, scoring_rules = sx_scoring) %>%
  add_ecr() %>%
  add_risk() %>%
  # add_adp() %>%
  add_player_info()


# Join Data ---------------------------------------------------------------

clt_projections_df <- clt_projections %>%
  left_join(clt_player_data %>%
              select(id, teamID),
            by = "id") %>% 
  mutate(position = case_when(
    position %in% c("CB", "S") ~ "DB",
    position %in% c("DT", "DE") ~ "DL",
    TRUE ~ position
  ))

sx_projections_df <- sx_projections %>% 
  left_join(sx_player_data %>% 
              select(id, teamID, is_injured),
            by = "id") #%>% 
  # filter(!injuryStatus %in% c("OUT", "INJURY RESERVE", "SUSPENSION"))

# Save Data ---------------------------------------------------------------
last_updated <- now()

save(clt_projections_df, sx_projections_df, 
     clt_player_data, sx_player_data,
     last_updated,
     file = here::here("ffanalytics", "projection-data.RData"))
