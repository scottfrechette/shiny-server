# export.R
# source() this at the end of your pipeline script, in place of save(...).
# Needs these objects from the pipeline: current_season, weeks_played,
# clt_rankings, clt_sim_standings_df, clt_current_matchups, clt_lines,
# clt_fvoa_season, clt_model_eval, clt_playoff_leverage, clt_schedule_luck,
# clt_lineup_eval.
#
# UNTESTED: written without access to fvoa's output structures. Run it locally
# once and check the JSON before wiring it into CI.

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
})

playoff_spots <- 6  # set to your league's playoff size

out_dir <- here::here("clt", "data", current_season,
                      sprintf("week-%02d", weeks_played))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Write a data frame as JSON. ggplots fall back to x$data as a stopgap:
# that is often NOT the data actually plotted (stats/transforms happen later),
# so replace those with real data functions in fvoa (see notes).
export_json <- function(x, name, required = FALSE) {
  if (inherits(x, "ggplot")) {
    warning(name, " is a ggplot; exporting x$data as a stopgap.", call. = FALSE)
    x <- x$data
  }
  if (!is.data.frame(x)) {
    msg <- paste0(name, " is not a data frame; not exported.")
    if (required) stop(msg, call. = FALSE)
    warning(msg, call. = FALSE)
    return(invisible(NULL))
  }
  write_json(x, file.path(out_dir, paste0(name, ".json")),
             dataframe = "rows", digits = 4, na = "null", auto_unbox = TRUE)
}

# Core files: fail the run if any of these is missing
export_json(clt_rankings,          "rankings",    required = TRUE)
export_json(clt_sim_standings_df,  "odds",        required = TRUE)
export_json(clt_current_matchups,  "matchups",    required = TRUE)
export_json(clt_lines,             "lines",       required = TRUE)
export_json(clt_fvoa_season,       "fvoa_season", required = TRUE)

# Secondary files: warn and continue
export_json(clt_model_eval,        "model_eval")
export_json(clt_playoff_leverage,  "leverage")
export_json(clt_schedule_luck,     "schedule_luck")
export_json(clt_lineup_eval,       "lineup_eval")

# Metadata + pointer so the frontend always knows where the latest week is
meta <- list(
  season = current_season,
  week = weeks_played,
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  fvoa_version = as.character(packageVersion("fvoa"))
)
write_json(meta, file.path(out_dir, "meta.json"), auto_unbox = TRUE)
write_json(
  list(season = current_season, week = weeks_played,
       path = sprintf("%s/week-%02d", current_season, weeks_played)),
  here::here("clt", "data", "latest.json"), auto_unbox = TRUE
)

# Facts for the commentary step -------------------------------------------

playoff_pct <- function(df) {
  df |>
    mutate(playoff_pct = rowSums(pick(all_of(as.character(1:playoff_spots))))) |>
    select(Team = team, playoff_pct)
}

facts <- clt_rankings |>
  select(Team, FVOA, fvoa_rank = `FVOA Rank`, yahoo_rank = `Yahoo Rank`,
         sor_rank = `SOR Rank`, sos_rank = `SOS Rank`) |>
  left_join(playoff_pct(clt_sim_standings_df), by = "Team") |>
  mutate(rank_gap = yahoo_rank - fvoa_rank,  # positive = model likes them more than Yahoo
         rank_change = NA_real_, playoff_change = NA_real_)

prev_dir <- here::here("clt", "data", current_season,
                       sprintf("week-%02d", weeks_played - 1))
has_prev <- file.exists(file.path(prev_dir, "rankings.json")) &&
  file.exists(file.path(prev_dir, "odds.json"))

if (has_prev) {
  prev_rank <- fromJSON(file.path(prev_dir, "rankings.json")) |>
    select(Team, prev_fvoa_rank = `FVOA Rank`)
  prev_odds <- fromJSON(file.path(prev_dir, "odds.json"), simplifyVector = TRUE) |>
    playoff_pct() |>
    rename(prev_playoff_pct = playoff_pct)
  facts <- facts |>
    left_join(prev_rank, by = "Team") |>
    left_join(prev_odds, by = "Team") |>
    mutate(rank_change = prev_fvoa_rank - fvoa_rank,   # positive = moved up
           playoff_change = playoff_pct - prev_playoff_pct) |>
    select(-prev_fvoa_rank, -prev_playoff_pct)
}

pick_team <- function(df, col, fun = max) {
  v <- df[[col]]
  if (all(is.na(v))) return(NULL)
  df$Team[which(abs(v) == fun(abs(v), na.rm = TRUE))[1]]
}

write_json(
  list(
    season = current_season,
    week = weeks_played,
    highlights = list(
      biggest_riser = if (has_prev) facts$Team[which.max(facts$rank_change)] else NULL,
      biggest_faller = if (has_prev) facts$Team[which.min(facts$rank_change)] else NULL,
      biggest_odds_swing = if (has_prev) pick_team(facts, "playoff_change") else NULL,
      biggest_model_disagreement = pick_team(facts, "rank_gap")
    ),
    teams = facts
  ),
  file.path(out_dir, "facts.json"), dataframe = "rows", digits = 4,
  na = "null", auto_unbox = TRUE
)
