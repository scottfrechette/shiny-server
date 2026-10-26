# read.R
# Drafts "The Read": one short paragraph per team, shown on each team page.
# The facts are computed here in R from the week's JSON snapshot; the model
# only phrases them. Output: clt/data/{season}/week-NN/read.json
#
#   Rscript clt/read.R
#
# Env vars:
#   ANTHROPIC_API_KEY   required
#   CLAUDE_MODEL        optional, defaults to claude-sonnet-5-5
#   FORCE_COMMENTARY=1  optional, regenerate even if read.json exists or the
#                       data is more than 48 hours old
#
# UNTESTED: run it locally before wiring it into CI.

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

model <- Sys.getenv("CLAUDE_MODEL", "claude-sonnet-5-5")
force <- nzchar(Sys.getenv("FORCE_COMMENTARY"))

# Locate the latest snapshot ------------------------------------------------

data_root <- here::here("clt", "data")
latest <- fromJSON(file.path(data_root, "latest.json"))
week_dir <- file.path(data_root, latest$path)
meta <- fromJSON(file.path(week_dir, "meta.json"))
out_json <- file.path(week_dir, "read.json")

age_hours <- as.numeric(difftime(
  Sys.time(),
  as.POSIXct(meta$generated_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  units = "hours"
))
if (!force && age_hours > 48) {
  message("Latest data is ", round(age_hours), " hours old (offseason?); skipping")
  quit(save = "no", status = 0)
}
if (!force && file.exists(out_json)) {
  message("read.json already exists; skipping (FORCE_COMMENTARY=1 to overwrite)")
  quit(save = "no", status = 0)
}
if (!nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
  stop("ANTHROPIC_API_KEY is not set", call. = FALSE)
}

# Compute the facts for each team -------------------------------------------

rd <- function(name) fromJSON(file.path(week_dir, paste0(name, ".json")))

rankings <- rd("rankings")
proj <- rd("projections")
luck <- rd("schedule_luck")
matchups <- rd("matchups")
lev <- rd("leverage")
facts <- rd("facts")$teams

# Each team's next game, from that team's point of view
next_game <- bind_rows(
  matchups |> transmute(Team = winner, opponent = loser,
                        win_prob_pct = round(winner_win_prob * 100), spread = spread),
  matchups |> transmute(Team = loser, opponent = winner,
                        win_prob_pct = round(loser_win_prob * 100), spread = -spread)
)

team_facts <- rankings |>
  transmute(Team, record = Record, pf = round(PF), pa = round(PA), fvoa = round(FVOA, 2),
            fvoa_rank = `FVOA Rank`, yahoo_rank = `Yahoo Rank`, sor_rank = `SOR Rank`,
            sos_rank = `SOS Rank`) |>
  mutate(pf_rank = rank(-pf, ties.method = "min"),
         pa_rank = rank(-pa, ties.method = "min")) |>  # 1 = most points scored against
  left_join(
    proj |> transmute(Team, playoff_odds_pct = round(playoff_pct * 100),
                      proj_wins = round(proj_wins, 1), proj_pts = round(proj_pf)),
    by = "Team") |>
  left_join(
    facts |> transmute(Team, rank_gap, playoff_change_pts = round(playoff_change * 100),
                       win_pct, pf_vs_avg_pct, record_vs_scoring),
    by = "Team") |>
  mutate(model_vs_standings = case_when(
    rank_gap >= 2 ~ "FVOA rates this team higher than its place in the standings",
    rank_gap <= -2 ~ "FVOA rates this team lower than its place in the standings",
    TRUE ~ "FVOA and the standings roughly agree")) |>
  left_join(
    luck |> transmute(Team, luck_label,
                      schedule_actual_rank = actual_rank,
                      schedule_expected_rank = round(expected_rank, 1),
                      pct_random_schedules_better = round(p_better * 100),
                      pct_random_schedules_worse = round(p_worse * 100)),
    by = "Team") |>
  left_join(next_game, by = "Team") |>
  left_join(
    lev |> transmute(Team, playoff_odds_if_win_pct = round(playoff_if_win * 100),
                     playoff_odds_if_lose_pct = round(playoff_if_lose * 100)),
    by = "Team")

facts_json <- as.character(toJSON(team_facts, dataframe = "rows", na = "null",
                                  auto_unbox = TRUE, digits = 2))
teams <- team_facts$Team

# Call the Claude API -------------------------------------------------------

read_text <- function(...) paste(readLines(here::here(...), warn = FALSE), collapse = "\n")
system_prompt <- read_text("clt", "commentary", "read_prompt.md")
user_msg <- paste0(
  "SEASON: ", latest$season, "\n",
  "WEEK: ", latest$week, " (games played)\n",
  "PLAYOFF SPOTS: ", meta$playoff_spots %||% 4, "\n\n",
  "DATA (one object per team)\n", facts_json
)

call_claude <- function(system, user, model, max_tokens = 16000) {
  resp <- request("https://api.anthropic.com/v1/messages") |>
    req_headers(
      `x-api-key` = Sys.getenv("ANTHROPIC_API_KEY"),
      `anthropic-version` = "2023-06-01"
    ) |>
    req_body_json(list(
      model = model,
      max_tokens = max_tokens,
      system = system,
      messages = list(list(role = "user", content = user))
    )) |>
    req_timeout(300) |>
    req_retry(max_tries = 3) |>
    req_perform()
  body <- resp_body_json(resp)
  message("stop_reason: ", body$stop_reason)
  blocks <- Filter(function(b) identical(b$type, "text"), body$content)
  paste(vapply(blocks, function(b) b$text, character(1)), collapse = "")
}

text <- call_claude(system_prompt, user_msg, model)
m <- regmatches(text, regexpr("(?s)\\{.*\\}", text, perl = TRUE))
clean <- if (length(m)) m else text
result <- tryCatch(
  fromJSON(clean, simplifyVector = FALSE),
  error = function(e) stop("Model did not return valid JSON:\n", text, call. = FALSE)
)

reads <- lapply(result$reads %||% list(), as.character)
missing <- setdiff(teams, names(reads))
if (length(missing)) {
  stop("Missing reads for: ", paste(missing, collapse = ", "), "\n", text, call. = FALSE)
}

# Validate ------------------------------------------------------------------
# Flags only: a human reviews the PR. These catch numbers that aren't in the data.

data_nums <- as.numeric(unlist(regmatches(facts_json, gregexpr("-?[0-9]+\\.?[0-9]*", facts_json))))

allowed_ranks <- function(tm) {
  r <- team_facts[team_facts$Team == tm, ]
  unique(c(r$fvoa_rank, r$yahoo_rank, r$sor_rank, r$sos_rank, r$pf_rank, r$pa_rank,
           r$schedule_actual_rank, floor(r$schedule_expected_rank),
           ceiling(r$schedule_expected_rank), meta$playoff_spots %||% 4))
}

flags <- character()
for (tm in teams) {
  txt <- reads[[tm]]
  
  n_words <- length(strsplit(trimws(txt), "\\s+")[[1]])
  if (n_words < 25 || n_words > 90) flags <- c(flags, sprintf("%s: %d words", tm, n_words))
  
  pcts <- as.numeric(unlist(regmatches(txt, gregexpr("[0-9]+\\.?[0-9]*(?=%)", txt, perl = TRUE))))
  bad_pct <- pcts[!vapply(pcts, function(p) any(abs(data_nums - p) <= 1.5, na.rm = TRUE), logical(1))]
  if (length(bad_pct)) {
    flags <- c(flags, sprintf("%s: percentages not in the data: %s", tm,
                              paste0(bad_pct, "%", collapse = ", ")))
  }
  
  ords <- as.numeric(unlist(regmatches(txt, gregexpr("[0-9]+(?=(st|nd|rd|th)\\b)", txt, perl = TRUE))))
  bad_ord <- ords[!ords %in% allowed_ranks(tm)]
  if (length(bad_ord)) {
    flags <- c(flags, sprintf("%s: ranks not in the data: %s", tm, paste(bad_ord, collapse = ", ")))
  }
}
extra <- setdiff(names(reads), teams)
if (length(extra)) flags <- c(flags, paste("Unknown team names:", paste(extra, collapse = ", ")))

# Write outputs -------------------------------------------------------------

out <- list(
  season = latest$season,
  week = latest$week,
  model = model,
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  reads = reads[teams],
  review_flags = flags
)
write_json(out, out_json, auto_unbox = TRUE, pretty = TRUE)

# Append to the PR description that commentary.R writes
md <- c("", sprintf("## The Read, week %d", latest$week), "",
        sprintf("- **%s**: %s", teams, unlist(reads[teams])))
if (length(flags)) md <- c(md, "", "### The Read review flags", paste0("- ", flags))
cat(paste0(md, "\n"), file = here::here("pr_body.md"), append = TRUE, sep = "")
cat(md, sep = "\n")