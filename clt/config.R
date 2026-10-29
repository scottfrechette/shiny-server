# config.R
# Loads clt/league.json, the one place league-specific settings live, and checks
# that nothing is missing. source() it near the top of each script.

cfg <- jsonlite::fromJSON(here::here("clt", "league.json"))

local({
  required <- c("league_name", "author_name", "commissioner_team", "yahoo_league_id",
                "season_week_offset", "regular_season_weeks", "last_data_week",
                "num_teams", "playoff_spots", "lineup_flex", "bench_labels",
                "scoring_band")
  missing <- setdiff(required, names(cfg))
  if (length(missing)) {
    stop("league.json is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
})

# Fill {{placeholders}} in prompt files from the config
fill_template <- function(text) {
  values <- list(
    league_name = cfg$league_name,
    num_teams = cfg$num_teams,
    author_name = cfg$author_name,
    commissioner_team = cfg$commissioner_team
  )
  for (k in names(values)) {
    text <- gsub(paste0("{{", k, "}}"), values[[k]], text, fixed = TRUE)
  }
  text
}
