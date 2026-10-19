# export.R
# source() this at the end of weekly.R (after the legacy save()), then source
# export_extra.R, which writes projections, schedule_luck, matchups and leverage.
#
# Needs these objects from the pipeline: current_season, weeks_played,
# clt_rankings, clt_sim_standings_df, clt_lines, clt_fvoa_season,
# clt_model_eval, clt_lineup_eval, clt_simulated_records.
#
# UNTESTED: written without access to fvoa's output structures. Run it locally
# once and check the JSON before relying on it in CI.

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
})

playoff_spots <- 4

out_dir <- here::here("clt", "data", current_season,
                      sprintf("week-%02d", weeks_played))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Write a data frame as JSON. ggplots fall back to x$data as a stopgap:
# that is often NOT the data actually plotted (stats/transforms happen later),
# so prefer a real data function in fvoa.
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
export_json(clt_rankings,         "rankings",    required = TRUE)
export_json(clt_sim_standings_df, "odds",        required = TRUE)  # finish-position odds only
export_json(clt_lines,            "lines",       required = TRUE)
export_json(clt_fvoa_season,      "fvoa_season", required = TRUE)

# Secondary files: warn and continue
export_json(clt_model_eval,       "model_eval")
export_json(clt_lineup_eval,      "lineup_eval")

# matchups, leverage, projections and schedule_luck are written by export_extra.R

# Metadata + pointer so the frontend always knows where the latest week is
meta <- list(
  season = current_season,
  week = weeks_played,
  playoff_spots = playoff_spots,
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

# Playoff odds come from clt_simulated_records, the same source as
# projections.json, so every page and the commentary agree. It also holds every
# earlier week, so the week-over-week change works without an old snapshot.
odds_now <- clt_simulated_records |>
  filter(week == weeks_played) |>
  select(Team = team, playoff_pct = playoffs)

odds_prev <- clt_simulated_records |>
  filter(week == weeks_played - 1) |>
  select(Team = team, prev_playoff_pct = playoffs)

facts <- clt_rankings |>
  select(Team, FVOA, fvoa_rank = `FVOA Rank`, yahoo_rank = `Yahoo Rank`,
         sor_rank = `SOR Rank`, sos_rank = `SOS Rank`) |>
  left_join(odds_now, by = "Team") |>
  left_join(odds_prev, by = "Team") |>
  mutate(rank_gap = yahoo_rank - fvoa_rank,  # positive = model likes them more than Yahoo
         playoff_change = playoff_pct - prev_playoff_pct,
         rank_change = NA_real_) |>
  select(-prev_playoff_pct)

# FVOA rank change still needs last week's snapshot
prev_rankings <- file.path(
  here::here("clt", "data", current_season, sprintf("week-%02d", weeks_played - 1)),
  "rankings.json"
)
has_prev <- file.exists(prev_rankings)

if (has_prev) {
  prev_rank <- fromJSON(prev_rankings) |>
    select(Team, prev_fvoa_rank = `FVOA Rank`)
  facts <- facts |>
    left_join(prev_rank, by = "Team") |>
    mutate(rank_change = prev_fvoa_rank - fvoa_rank) |>  # positive = moved up
    select(-prev_fvoa_rank)
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
      biggest_odds_swing = pick_team(facts, "playoff_change"),
      biggest_model_disagreement = pick_team(facts, "rank_gap")
    ),
    teams = facts
  ),
  file.path(out_dir, "facts.json"), dataframe = "rows", digits = 4,
  na = "null", auto_unbox = TRUE
)