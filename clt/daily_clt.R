oldw <- getOption("warn")
options(warn = -1)

# Load packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(fvoa))


# Temp Functions -----------------------------------------------------------

all_matchups1 <- function (scores, type = "prob", reg_games = 6, reps = 1e+06, 
                           matrix = FALSE) 
{
  teams <- scores %>% distinct(Team)
  tmp <- teams %>% rename(Team1 = Team) %>% crossing(teams) %>% filter(Team != Team1) %>% 
    mutate(data = list(scores), type = type)
  if (type == "prob") {
    matchups <- tmp %>% mutate(x = pmap_dbl(list(data, Team, 
                                                 Team1, type = type), matchup)) %>% 
      select(-data, -type) %>% 
      spread(Team1, x) %>% 
      mutate_if(is.numeric, replace_na, 0)
  }
  else {
    matchups <- tmp %>% mutate(x = pmap_chr(list(data, Team, 
                                                 Team1, type = type), matchup)) %>% 
      select(-data, -type) %>% 
      spread(Team1, x) %>% 
      mutate_if(is.character, replace_na, 0)
  }
  if (matrix) {
    matchups <- as.matrix(matchups)
    row.names(matchups) <- matchups[, 1]
    matchups <- matchups[, -1]
  }
  matchups
}

calculate_fvoa1 <- function (scores) 
{
  all_matchups1(scores, type = "prob") %>% select(-Team) %>% 
    map_df(function(x) {
      round((mean(100 - x, na.rm = T) - 50)/0.5, 2)
    }) %>% gather(Team, FVOA) %>% arrange(-FVOA) %>% mutate(`FVOA Rank` = dense_rank(-FVOA), 
                                                            Week = max(scores$Week))
}

calculate_fvoa_season1 <- function (scores) 
{
  fvoa <- list()
  for (i in 1:n_distinct(scores$Week)) {
    scores_tmp <- scores %>% filter(Week %in% 1:i)
    fvoa_rankings <- calculate_fvoa1(scores_tmp)
    fvoa[i] <- list(fvoa_rankings)
  }
  data_frame(fvoa) %>% unnest()
}

calculate_strength_schedule1 <- function (schedule, scores) 
{
  if ("Team" %in% names(schedule)) {
    schedule <- spread_schedule(schedule) %>% doublewide_schedule()
  }
  calculate_fvoa1(scores) %>% select(Team2 = Team, FVOA) %>% 
    right_join(schedule, by = "Team2") %>% rename(Team = Team1) %>% 
    group_by(Team) %>% summarise(SoS = round(mean(FVOA), 
                                             2)) %>% arrange(SoS) %>% mutate(`SoS Rank` = min_rank(-SoS))
}

calculate_rankings1 <- function (schedule, scores) 
{
  calculate_stats(schedule, scores) %>% 
    left_join(calculate_fvoa1(scores) %>% 
                select(-Week), by = "Team") %>%
    left_join(calculate_strength_schedule1(schedule, scores), by = "Team") %>% 
    left_join(calculate_colley(schedule, scores), by = "Team")
}

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
    scale_y_continuous(limits = c(0, 105), breaks = c(0, 25, 50, 75, 100)) + 
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


# Script ------------------------------------------------------------------


# Identify current week
today_week <- today() %>%
  floor_date(unit = "week", week_start = 2) %>%
  week()
start_week <- 35
current_week <- today_week - start_week
weeks_played <- current_week - 1

# Scrape data
clt_schedule <- scrape_schedule("yahoo", 96662)
clt_team <- scrape_team(weeks_played, "yahoo", 96662) %>% 
  unnest()
clt_yahoo_win_prob <- scrape_win_prob(current_week, "yahoo", 96662)

# Replace team names
source(here::here("clt", "lookup_id.R"))
team_ids <- yahoo_teamIDs(96662) %>% 
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
clt_scores <- clt_proj %>% 
  select(-Proj)


# Run FVOA analysis
clt_simulated_season <- simulate_season(clt_schedule, clt_scores, "clt") 
playoff_leverage <- read_csv(here::here("clt", "playoff_leverage.csv"))
clt_model_eval <- evaluate_model(clt_scores, output = "shiny")
clt_fvoa_season <- calculate_fvoa_season1(clt_scores)
clt_matchups_prob <- all_matchups(clt_scores, type = "prob")
clt_matchups_spread <- all_matchups(clt_scores, type = "spread")
clt_rankings <- calculate_rankings1(clt_schedule, clt_scores)
clt_current_matchups <- current_matchups(current_week, clt_schedule, 
                                         clt_scores, clt_yahoo_win_prob)
clt_playoff_leverage_chart <- playoff_leverage_plot1(clt_scores, clt_schedule, 
                                                    playoff_leverage)
clt_lineup_eval <- evaluate_lineup(clt_team, flex = 0, plot = T)

# Save data for Shiny app
save(clt_schedule, clt_team, clt_proj, clt_scores,
     clt_simulated_season, clt_model_eval, clt_fvoa_season,
     clt_matchups_prob, clt_matchups_spread,
     clt_rankings, clt_current_matchups,
     clt_playoff_leverage_chart, clt_lineup_eval, 
     file = here::here("clt", "clt-data.RData"))

options(warn = oldw)