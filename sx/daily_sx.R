
# Old ---------------------------------------------------------------------



oldw <- getOption("warn")
options(warn = -1)

# Load packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
# suppressPackageStartupMessages(library(rvest))
# suppressPackageStartupMessages(library(here))
# suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(fvoa))

# Identify current week
today_week <- today() %>%
  floor_date(unit = "week", week_start = 2) %>%
  week()
start_week <- 35
current_week <- today_week - start_week
weeks_played <- current_week - 1

source(here::here("sx", "sx_lookupIDs.R"))

# Scrape data
scrape_schedule_espn <- function(seasonID = 2020, leagueID = 299999) {
  
  if(seasonID == as.numeric(format(Sys.Date(),'%Y'))) {
    
    url <- str_glue("https://fantasy.espn.com/apis/v3/games/ffl/seasons/{seasonID}/segments/0/leagues/{leagueID}?view=mMatchupScore")
    scores <- fromJSON(url) %>%
      .$schedule
    
    # explore scores$away$rosterForCurrentScoringPeriod$entries after week 1
    
    bind_cols(id = scores$id,
              week = scores$matchupPeriodId,
              scores$home %>%
                select(team1 = teamId, team1_points = totalPoints),
              scores$away %>%
                select(team2 = teamId, team2_points = totalPoints),
              playoff = scores$playoffTierType) %>%
      mutate(game = if_else(playoff == "NONE", "regular", "playoff")) %>%
      select(-playoff)
    
  } else {
    
    url <- str_glue("https://fantasy.espn.com/apis/v3/games/ffl/leagueHistory/{leagueID}?seasonId={seasonID}&view=mMatchupScore")
    scores <- fromJSON(url) %>%
      .$schedule %>%
      .[[1]] %>%
      map_if(is.data.frame, list) %>%
      as_tibble()
    
    bind_cols(id = scores$id,
              week = scores$matchupPeriodId,
              scores$home[[1]] %>%
                select(team1 = teamId, team1_points = totalPoints),
              scores$away[[1]] %>%
                select(team2 = teamId, team2_points = totalPoints),
              playoff = scores$playoffTierType) %>%
      mutate(id = row_number(),
             game = if_else(playoff == "NONE", "regular", "playoff")) %>%
      select(-playoff)
    
  }
  
}
sx_league <- scrape_schedule_espn()

sx_schedule <- sx_league %>% 
  filter(game == "regular") %>% 
  select(Week = week, Game_id = id, team1, team2) %>% 
  gather(key, teamID, team1, team2) %>% 
  select(-key) %>% 
  left_join(sx_lookup_ids, by = "teamID") %>% 
  select(Week, Game_id, Team = team)

scrape_weekly <- function(seasonID = 2020, leagueID = 299999, week = 1) {
  
  if(seasonID == as.numeric(format(Sys.Date(),'%Y'))) {
    
    url <- str_glue("https://fantasy.espn.com/apis/v3/games/ffl/seasons/{seasonID}/segments/0/leagues/{leagueID}?view=mMatchup&view=mMatchupScore&scoringPeriodId={week}")
    
    teams <- fromJSON(url) %>%
      .$teams %>%
      map_if(is.data.frame, list) %>%
      as_tibble()
    
    team_roster <- teams$roster[[1]]$entries
    
  } else {
    
    url <- str_glue("https://fantasy.espn.com/apis/v3/games/ffl/leagueHistory/{leagueID}?seasonId={seasonID}&view=mMatchup&view=mMatchupScore&scoringPeriodId={week}")
    
    
    teams <- fromJSON(url) %>%
      .$teams %>%
      .[[1]] %>%
      map_if(is.data.frame, list) %>%
      as_tibble()
    
    team_roster <- teams$roster[[1]]$entries
    
  }
  
  if(seasonID >= 2017) {
    
    teams %>%
      select(teamID = id) %>%
      mutate(
        season = seasonID,
        score = teams$roster[[1]]$appliedStatTotal,
        team_n = row_number(),
        player_data = map(team_n,
                          ~ team_roster[[.x]][["playerPoolEntry"]][["player"]] %>%
                            as_tibble() %>%
                            select(playerID = id, player = fullName,
                                   position = defaultPositionId, proTeamId,
                                   injured, injuryStatus)),
        roster = map(team_n,
                     ~team_roster[[.x]][["lineupSlotId"]])) %>%
      unnest(c(player_data, roster)) %>%
      group_by(teamID) %>%
      mutate(
        position = case_when(
          position == 1 ~ "QB",
          position == 2 ~ "RB",
          position == 3 ~ "WR",
          position == 4 ~ "TE",
          position == 5 ~ "K",
          position == 16 ~ "DST"),
        roster = case_when(
          roster == 0 ~ "QB",
          roster == 2 ~ "RB",
          roster == 3 ~ "RB/WR",
          roster == 4 ~ "WR",
          roster == 6 ~ "TE",
          roster == 16 ~ "DST",
          roster == 17 ~ "K",
          roster == 20 ~ "BN",
          roster == 23 ~ "FLEX"
        ),
        data = map2(team_n, row_number(),
                    ~ team_roster[[.x]][["playerPoolEntry"]][["player"]][["stats"]][[.y]] %>%
                      map_if(is.data.frame, list) %>%
                      as_tibble() %>%
                      filter(seasonId == seasonID, scoringPeriodId > 0) %>%
                      mutate(type = if_else(statSourceId == 1, "proj", "act")) %>%
                      select(scoringPeriodId, type, points = appliedTotal) %>%
                      spread(type, points))
      ) %>%
      ungroup() %>%
      unnest(c(data)) %>%
      filter(scoringPeriodId == week) %>%
      select(season, week = scoringPeriodId, teamID, score,
             playerID:position, roster,
             proj, act, injured, injuryStatus)
    
  } else {
    
    teams %>%
      unite(team, location, nickname, sep = " ") %>%
      mutate(team = str_trim(team)) %>%
      select(teamID = id, team) %>%
      mutate(
        season = seasonID,
        score = teams$roster[[1]]$appliedStatTotal,
        team_n = row_number(),
        player_data = map(team_n,
                          ~ team_roster[[.x]][["playerPoolEntry"]][["player"]] %>%
                            as_tibble() %>%
                            select(playerID = id, player = fullName,
                                   position = defaultPositionId, proTeamId,
                                   injured)),
        roster = map(team_n,
                     ~team_roster[[.x]][["lineupSlotId"]])
      ) %>%
      unnest(c(player_data, roster)) %>%
      group_by(teamID) %>%
      mutate(
        position = case_when(
          position == 1 ~ "QB",
          position == 2 ~ "RB",
          position == 3 ~ "WR",
          position == 4 ~ "TE",
          position == 5 ~ "K",
          position == 16 ~ "DST"),
        roster = case_when(
          roster == 0 ~ "QB",
          roster == 2 ~ "RB",
          roster == 3 ~ "RB/WR",
          roster == 4 ~ "WR",
          roster == 6 ~ "TE",
          roster == 16 ~ "DST",
          roster == 17 ~ "K",
          roster == 20 ~ "BN",
          roster == 23 ~ "FLEX"
        ),
        data = map2(team_n, row_number(),
                    ~ team_roster[[.x]][["playerPoolEntry"]][["player"]][["stats"]][[.y]] %>%
                      map_if(is.data.frame, list) %>%
                      as_tibble() %>%
                      filter(seasonId == seasonID, scoringPeriodId > 0) %>%
                      mutate(type = if_else(statSourceId == 1, "proj", "act")) %>%
                      select(scoringPeriodId, type, points = appliedTotal) %>%
                      spread(type, points))
      ) %>%
      ungroup() %>%
      unnest(c(data)) %>%
      filter(scoringPeriodId == week) %>%
      select(season, week = scoringPeriodId, teamID, score,
             playerID:position, roster,
             proj, act, injury)
    
  }
}
sx_team <- tibble(Week = 1:weeks_played) %>% 
  mutate(data = map(Week, ~ scrape_weekly(week = .x))) %>% 
  unnest(data) %>% 
  left_join(sx_lookup_ids, by = "teamID") %>% 
  mutate(pts = if_else(roster != "BN", act, 0)) %>% 
  add_count(team, Week, wt = pts, name = "Score") %>% 
  select(teamID, Week, Team = team, 
         # Score = score,
         Score, Player = player,
         Position = position, Lineup = roster, 
         Proj = proj, Points = act)

# Extract scores
sx_scores <- bind_rows(sx_league %>% 
                         select(Week = week, teamID = team1, Score = team1_points),
                       sx_league %>% 
                         select(Week = week, teamID = team2, Score = team2_points)) %>% 
  filter(Week <= weeks_played) %>% 
  arrange(Week, teamID) %>% 
  left_join(sx_lookup_ids, by = "teamID") %>% 
  select(Week, Team = team, Score)


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
playoff_leverage_plot1 <- function (scores, schedule, playoff_leverage_df) 
{
  sims <- max(playoff_leverage_df$sim)
  if ("Team" %in% names(schedule)) {
    schedule <- schedule %>% spread_schedule() %>% doublewide_schedule()
  }
  schedule %>% 
    filter(Week == max(scores$Week) + 1) %>% 
    mutate(tmp = list(playoff_leverage_df)) %>% 
    unnest(tmp) %>% 
    mutate(sim_wins = Wins, 
           wins_with_loss = pl_wins, 
           wins_with_win = pl_wins + 1L, 
           wins = case_when(Team == Team1 ~ wins_with_win,
                            Team == Team2 ~ wins_with_loss, 
                            TRUE ~ sim_wins)) %>% 
    # select(Week:Team, sim, wins) %>% 
    arrange(-wins) %>% 
    group_by(Team1, sim) %>%
    mutate(row = row_number(), 
           playoff = if_else(row <= 4, 1, 0)) %>%
    ungroup() %>% 
    group_by(Team1, Team) %>% 
    summarise(Percent = sum(playoff/sims * 100)) %>% 
    ungroup() %>% 
    arrange(Team1, -Percent) %>% 
    rename(Winner = Team1) %>% 
    nest(Team:Percent) %>% 
    left_join(schedule %>% 
                filter(Week == max(scores$Week) + 1) %>% 
                select(-Week) %>% 
                rename(Winner = Team1, Loser = Team2), 
              by = "Winner") %>% 
    select(Winner, Loser, data) %>% 
    unnest() %>% 
    filter(Winner == Team | Loser == Team) %>% 
    arrange(Team) %>% 
    mutate(style = if_else(Winner == Team, "Win", "Lose")) %>% 
    select(Team:style) %>% 
    spread(style, Percent) %>% 
    mutate(Delta = round(Win - Lose, 1), Total = 100) %>% 
    ggplot(aes(reorder(Team, Win), y = Total)) + 
    geom_bar(stat = "identity", 
             fill = "white", color = "grey", alpha = 0.4) + 
    geom_bar(stat = "identity", aes(y = Win, fill = Team), alpha = 0.5) + 
    geom_bar(stat = "identity", aes(y = Lose, fill = Team)) +
    geom_text(aes(y = Total + 0.5, label = paste0(Delta, "%")), 
              color = "grey30", hjust = 0) + 
    scale_y_continuous(limits = c(0, 105), 
                       breaks = c(0, 25, 50, 75, 100),
                       expand = expansion(mult = c(0, 0.1))) + 
    guides(fill = FALSE) +
    labs(x = "",  y = "Chance to Make Playoffs", 
         title = str_glue("Playoff Probability Leverage (Week {max(scores$Week) + 1})")) + 
    coord_flip() + 
    theme(plot.title = element_text(hjust = 0.5), 
          panel.background = element_blank(), panel.border = element_blank(), 
          strip.background = element_rect(color = "black"), panel.grid = element_blank(), 
          panel.grid.major.x = element_line(color = "grey90", size = 0.2), 
          strip.text = element_text(size = 12))
}
sx_playoff_leverage_chart <- playoff_leverage_plot1(sx_scores, sx_schedule,
                                                    playoff_leverage)
sx_lineup_eval <- evaluate_lineup(sx_team, wr = 2, dl = 0, db = 0, plot = T)

sx_simulated_season <- sx_simulated_season %>% filter(Week != 0)

# Save data for Shiny app
save(sx_schedule, sx_team, sx_scores,
     sx_simulated_season, sx_model_eval,
     sx_fvoa_season,
     sx_matchups_prob, sx_matchups_spread,
     sx_rankings, sx_current_matchups,
     sx_playoff_leverage_chart, 
     sx_lineup_eval, 
     file = here::here("sx", "sx-data.RData"))

options(warn = oldw)


# New ---------------------------------------------------------------------

oldw <- getOption("warn")
options(warn = -1)

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

sx_con <- dbConnect(SQLite(), here::here("sx", "sx.sqlite"))

sx_team_tmp <- scrape_team("espn", 299999, weeks_played)

if(exists("sx_team_tmp")) {
  
  dbSendQuery(sx_con, str_glue("DELETE from teams where week == {weeks_played}"))
  
  dbWriteTable(sx_con, 
               "teams", sx_team_tmp, 
               overwrite = FALSE, append = TRUE)
  
  rm(sx_team_tmp)
  
}

sx_owners <- collect(tbl(sx_con, "owners"))
sx_schedule <- collect(tbl(sx_con, "schedule"))
sx_team <- collect(tbl(sx_con, "teams"))
sx_scores <- extract_scores(sx_team)
sx_proj <- extract_projections(sx_team)

sx_simulated_scores_tmp <- simulate_season(sx_team, 1000, max(sx_schedule$week))

if(exists("sx_simulated_scores_tmp")) {
  
  dbSendQuery(sx_con, str_glue("DELETE from simulated_scores where week == {weeks_played}"))
  
  dbWriteTable(sx_con, 
               "simulated_scores", sx_simulated_scores_tmp, 
               overwrite = FALSE, append = TRUE)
  
}

sx_simulated_season_tmp <- simulate_record(sx_simulated_scores_tmp,
                                           sx_schedule,
                                           max(sx_team$week)) %>% 
  simulate_ranking(max(sx_team$week))

if(exists("sx_simulated_season_tmp")) {
  
  dbSendQuery(sx_con, str_glue("DELETE from simulated_seasons where week == {weeks_played}"))
  
  dbWriteTable(sx_con, 
               "simulated_seasons", sx_simulated_season_tmp, 
               overwrite = FALSE, append = TRUE)
  
  rm(sx_simulated_scores_tmp, sx_simulated_season_tmp)
  
}

sx_model_eval_tmp <- evaluate_model(sx_team, weeks_played, .reps = 1e6)

if(exists("sx_model_eval_tmp")) {
  
  dbSendQuery(sx_con, str_glue("DELETE from model_evaluation where week == {weeks_played}"))
  
  dbWriteTable(sx_con, 
               "model_evaluation", sx_model_eval_tmp, 
               overwrite = FALSE, append = TRUE)
  
  rm(sx_model_eval_tmp)
  
}

sx_players_tmp <- scrape_player_projections("espn", 299999, week = weeks_played, season = 2020)

if(exists("sx_players_tmp")) {
  
  dbSendQuery(sx_con, str_glue("DELETE from players where week == {weeks_played}"))
  
  dbWriteTable(sx_con, 
               "players", sx_players_tmp, 
               overwrite = FALSE, append = TRUE)
  
  rm(sx_players_tmp)
  
}

sx_schedule <- sx_schedule %>% 
  fvoa:::gather_schedule() %>% 
  mutate(teamID = as.integer(teamID)) %>% 
  left_join(sx_owners, 
            by = c("league", "leagueID", "teamID")) %>% 
  select(-teamID) %>% 
  fvoa:::spread_schedule()
sx_team <- sx_team %>% 
  left_join(sx_owners, 
            by = c("league", "leagueID", "teamID")) %>% 
  select(league:week, team, score:act_pts)
sx_scores <- extract_scores(sx_team)
sx_proj <- extract_projections(sx_team)
sx_model_eval <- collect(tbl(sx_con, "model_evaluation")) %>% 
  left_join(sx_owners, 
            by = c("league", "leagueID", "teamID")) %>% 
  select(league:week, team, opp:sim)
sx_simulated_season <- collect(tbl(sx_con, "simulated_seasons")) %>% 
  left_join(sx_owners, 
            by = c("league", "leagueID", "teamID")) %>% 
  select(league:week, team, points:rank)
sx_simulated_scores <- collect(tbl(sx_con, "simulated_scores")) %>% 
  left_join(sx_owners, 
            by = c("league", "leagueID", "teamID")) %>% 
  select(league:sim_week, team, score)

sx_playoff_leverage <- simulate_record(sx_simulated_scores, sx_schedule, weeks_played)
sx_fvoa_season <- calculate_fvoa_season(sx_scores)
sx_matchups_prob <- compare_league(sx_scores, .reps = 1e6) %>%
  fvoa:::spread_league(.output = "wp")
sx_matchups_spread <- compare_league(sx_scores, .reps = 1e6) %>% 
  fvoa:::spread_league(.output = "spread")
sx_rankings <- calculate_rankings(sx_schedule, sx_scores, "espn")
sx_current_matchups <- compare_current_matchups(sx_scores, 
                                                sx_schedule, 
                                                current_week, 
                                                .reps = 1e6)
sx_playoff_leverage_chart <- plot_playoff_leverage(sx_scores, 
                                                   sx_schedule, 
                                                   sx_playoff_leverage)
sx_lineup_eval <- evaluate_lineup(sx_team, flex = 0, plot = TRUE)

save(sx_schedule, sx_team, sx_proj, sx_scores,
     sx_simulated_season, sx_model_eval, 
     sx_fvoa_season,
     sx_matchups_prob, sx_matchups_spread,
     sx_rankings, sx_current_matchups,
     sx_playoff_leverage_chart, sx_lineup_eval, 
     file = here::here("sx", "sx-data.RData"))

