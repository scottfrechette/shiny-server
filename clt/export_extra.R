# export_extra.R
# source() this right after export.R in weekly.R. It reuses out_dir,
# export_json() and weeks_played, and needs clt_simulated_records and
# clt_schedule_luck from the pipeline.
#
# UNTESTED: written from glimpse() output only. Run locally and check the JSON.

suppressPackageStartupMessages(library(dplyr))

# Projections: Odds screen (wins, points) and My Team tiles -------------------

history <- as_tibble(clt_simulated_records)

projections <- history |>
  filter(week == weeks_played) |>
  transmute(
    Team = team,
    proj_wins = wins,
    proj_losses = losses,
    proj_pf = pf,            # full-season projected points for
    proj_pa = pa,            # full-season projected points against
    proj_win_pct = wp,
    playoff_pct = playoffs,  # fraction, 0.25 = 25%
    proj_final_rank = rank
  )

if (nrow(projections) == 0) {
  stop("clt_simulated_records has no rows for week ", weeks_played, call. = FALSE)
}

export_json(projections, "projections", required = TRUE)
export_json(history, "records_history")  # every week's projections, for trend charts

# Schedule luck: "The Read" ---------------------------------------------------
# clt_schedule_luck$data has one row per team x finishing rank, where pct is the
# share of simulated random schedules that end with the team at that rank.
# sim_rank is the single most common finish (the mode), which can sit well away
# from the average when the distribution is wide, so it is NOT used for luck.
# Instead, compare the whole distribution to the team's actual rank.

luck_dist <- clt_schedule_luck$data |>
  as_tibble() |>
  mutate(team = as.character(team))

schedule_luck <- luck_dist |>
  summarise(
    actual_rank = first(actual_rank),
    expected_rank = sum(rank * pct),                 # average finish across random schedules
    p_better = sum(pct[rank < first(actual_rank)]),  # random schedules that leave the team ranked higher
    p_same = sum(pct[rank == first(actual_rank)]),
    p_worse = sum(pct[rank > first(actual_rank)]),   # random schedules that leave the team ranked lower
    .by = team
  ) |>
  mutate(
    luck_score = p_worse - p_better,  # -1 to 1; positive = lucky (most random schedules would be worse)
    luck_label = case_when(           # thresholds are a judgment call; tune once you see the real spread
      luck_score >= 0.3 ~ "lucky",
      luck_score <= -0.3 ~ "unlucky",
      TRUE ~ "about even"
    )
  ) |>
  rename(Team = team)

export_json(schedule_luck, "schedule_luck", required = TRUE)
export_json(luck_dist, "schedule_luck_dist")

# Matchups: Week screen ---------------------------------------------------
# clt_current_matchups describes the UPCOMING games (current_week is
# weeks_played + 1), so the site should label this "Week {week}", not the
# week of the folder it sits in. Values arrive as strings ("58%", "-6.5",
# "+100"), so convert them. "winner" is the listed side of each game; its FVOA
# win probability can be under 50% (a pick'em), so don't call it the favorite
# without checking.

matchups <- clt_current_matchups |>
  as_tibble() |>
  transmute(
    week = current_week,
    winner = Winner,
    loser = Loser,
    winner_win_prob = as.numeric(sub("%", "", FVOA)) / 100,
    loser_win_prob = 1 - winner_win_prob,
    spread = as.numeric(Spread),  # negative = winner is laying points
    moneyline = as.numeric(Line),
    quality = Quality,
    importance = Importance,
    overall = Overall             # about the average of quality and importance
  )

export_json(matchups, "matchups", required = TRUE)

# Playoff leverage: "if you win, your odds go ..." ------------------------
# Assumes these are for the upcoming game, like the matchups above.

leverage <- clt_playoff_leverage$data |>
  as_tibble() |>
  transmute(
    Team = as.character(team),
    playoff_if_win = Win,
    playoff_if_lose = Lose,
    leverage = delta  # Win minus Lose: how much the next game swings playoff odds
  )

export_json(leverage, "leverage", required = TRUE)

# Trends: weekly score and FVOA by team ------------------------------------
# One row per team per week. The team page reads this for the "Weekly score"
# and "FVOA" trend charts.

trends <- as_tibble(clt_scores) |>
  select(Team = team, week, score) |>
  full_join(
    as_tibble(clt_fvoa_season) |> select(Team = team, week, fvoa),
    by = c("Team", "week")
  ) |>
  arrange(week, Team)

export_json(trends, "trends", required = TRUE)

# Bench points: points left on the bench ------------------------------------
# points_left = the best lineup the team could have started minus what it
# started, from fvoa::evaluate_lineup(). Wrapped so a problem here warns and
# skips instead of failing the whole weekly run.

bench_labels <- cfg$bench_labels  # roster labels that mean "not started"

build_bench <- function() {
  rosters <- as_tibble(clt_team) |> mutate(points = coalesce(points, 0))
  
  # Sanity check: the starters' points should add up to the team score
  check <- rosters |>
    summarise(starters = sum(points[!roster %in% bench_labels]),
              score = first(score), .by = c(team, week))
  if (max(abs(check$starters - check$score)) > 1) {
    stop("starters' points do not match team scores. Roster labels seen: ",
         paste(sort(unique(rosters$roster)), collapse = ", "),
         ". Update bench_labels to the ones that mean bench.", call. = FALSE)
  }
  
  lineups <- as_tibble(evaluate_lineup(as_tibble(clt_team), flex = cfg$lineup_flex)) |>
    mutate(points_left = pmax(round(optimal - actual, 1), 0))
  
  weekly <- lineups |> select(Team = team, week, optimal, actual, points_left)
  
  summary <- lineups |>
    summarise(points_left_season = sum(points_left),
              points_left_week = sum(points_left[week == weeks_played]),
              .by = team) |>
    mutate(points_left_week_rank = rank(-points_left_week, ties.method = "min"),
           points_left_season_rank = rank(-points_left_season, ties.method = "min"))
  
  # Biggest single miss in the latest week: the bench player who most outscored
  # the lowest-scoring starter at his position
  latest <- rosters |> filter(week == weeks_played)
  floors <- latest |>
    filter(!roster %in% bench_labels) |>
    summarise(starter_points = min(points), outscored_player = player[which.min(points)],
              .by = c(team, position))
  misses <- latest |>
    filter(roster %in% bench_labels) |>
    inner_join(floors, by = c("team", "position")) |>
    mutate(margin = points - starter_points) |>
    filter(margin > 0) |>
    slice_max(margin, n = 1, by = team, with_ties = FALSE) |>
    transmute(team, bench_miss_player = player, bench_miss_position = position,
              bench_miss_points = round(points, 1), outscored_player,
              outscored_points = round(starter_points, 1))
  
  bench <- summary |>
    left_join(misses, by = "team") |>
    mutate(across(c(points_left_week, points_left_season), \(v) round(v, 1))) |>
    rename(Team = team)
  
  export_json(bench, "bench")
  export_json(weekly, "bench_weekly")
  # Used only by the commentary scripts, to flag player names the model should not use
  write_json(sort(unique(clt_team$player)), file.path(out_dir, "roster_names.json"))
}

tryCatch(
  build_bench(),
  error = function(e) warning("Bench points skipped: ", conditionMessage(e), call. = FALSE)
)