# commentary.R
# Drafts the weekly FVOA Takes from the JSON snapshots written by weekly.R.
#
#   Rscript clt/commentary.R
#
# Env vars:
#   ANTHROPIC_API_KEY   required
#   CLAUDE_MODEL        optional, defaults to claude-sonnet-5-5
#   FORCE_COMMENTARY=1  optional, regenerate even if commentary.json exists
#                       or the data is more than 48 hours old
#
# UNTESTED: run it locally once before wiring it into CI.

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

reg_season_weeks <- 15
commish_team <- "Barrett"
model <- Sys.getenv("CLAUDE_MODEL", "claude-sonnet-5-5")
force <- nzchar(Sys.getenv("FORCE_COMMENTARY"))

# Locate the latest snapshot ------------------------------------------------

data_root <- here::here("clt", "data")
latest <- fromJSON(file.path(data_root, "latest.json"))
week_dir <- file.path(data_root, latest$path)
week <- latest$week
meta <- fromJSON(file.path(week_dir, "meta.json"))
out_json <- file.path(week_dir, "commentary.json")

# Skip checks ---------------------------------------------------------------

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
  message("commentary.json already exists; skipping (FORCE_COMMENTARY=1 to overwrite)")
  quit(save = "no", status = 0)
}
if (!nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
  stop("ANTHROPIC_API_KEY is not set", call. = FALSE)
}

mode <- if (week == 1) "opener" else if (week == reg_season_weeks) "season_end" else "weekly"
playoff_spots <- meta$playoff_spots %||% 4

# Build the prompt ----------------------------------------------------------

read_raw <- function(name) {
  f <- file.path(week_dir, paste0(name, ".json"))
  if (!file.exists(f)) return(NULL)
  minify(paste(readLines(f, warn = FALSE), collapse = "\n"))
}

inputs <- c("facts", "rankings", "odds", "matchups", "model_eval",
            if (mode == "season_end") "fvoa_season")
raw <- Filter(Negate(is.null), setNames(lapply(inputs, read_raw), inputs))

data_block <- paste0("### ", names(raw), ".json\n", unlist(raw), collapse = "\n\n")
user_msg <- paste0(
  "MODE: ", mode, "\n",
  "SEASON: ", latest$season, "\n",
  "WEEK: ", week, " (games played)\n",
  "REGULAR SEASON LENGTH: ", reg_season_weeks, " weeks\n",
  "PLAYOFF SPOTS: ", playoff_spots, "\n",
  "COMMISH TEAM: ", commish_team, "\n\n",
  "DATA\n\n", data_block
)

read_text <- function(...) paste(readLines(here::here(...), warn = FALSE), collapse = "\n")
system_prompt <- paste0(
  read_text("clt", "commentary", "prompt.md"),
  "\n\n# VOICE EXAMPLES\n",
  "Past posts, newest first. Imitate the tone and structure. ",
  "Never reuse their facts, numbers, or jokes.\n\n",
  read_text("clt", "commentary", "voice_examples.md")
)

# Call the Claude API -------------------------------------------------------

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
  message("stop_reason: ", body$stop_reason, " | blocks: ",
          paste(vapply(body$content, function(b) b$type, character(1)), collapse = ", "))
  blocks <- Filter(function(b) identical(b$type, "text"), body$content)
  paste(vapply(blocks, function(b) b$text, character(1)), collapse = "")
}

text <- call_claude(system_prompt, user_msg, model)
# clean <- gsub("^```(json)?\\s*|\\s*```$", "", trimws(text))
m <- regmatches(text, regexpr("(?s)\\{.*\\}", text, perl = TRUE))
clean <- if (length(m)) m else text
result <- tryCatch(
  fromJSON(clean, simplifyVector = FALSE),
  error = function(e) stop("Model did not return valid JSON:\n", text, call. = FALSE)
)

# Validate ------------------------------------------------------------------

if (!length(result$bullets) || !nzchar(result$commish_corner %||% "")) {
  stop("Response is missing bullets or commish_corner:\n", text, call. = FALSE)
}

teams <- fromJSON(file.path(week_dir, "rankings.json"))$Team
flags <- character()

if (!length(result$bullets) %in% 4:6) {
  flags <- c(flags, paste0("Unexpected bullet count: ", length(result$bullets)))
}

if (mode == "season_end") {
  award_teams <- vapply(result$awards %||% list(), function(a) a$team %||% "", character(1))
  missing <- setdiff(teams, award_teams)
  extra <- setdiff(award_teams, teams)
  if (length(missing)) flags <- c(flags, paste("Missing awards for:", paste(missing, collapse = ", ")))
  if (length(extra)) flags <- c(flags, paste("Award for unknown team:", paste(extra, collapse = ", ")))
}

# Every percentage in the text should be traceable to a number in the data
data_nums <- as.numeric(unlist(regmatches(
  paste(unlist(raw), collapse = " "),
  gregexpr("-?[0-9]+\\.?[0-9]*", paste(unlist(raw), collapse = " "))
)))
out_txt <- paste(
  c(unlist(result$bullets), result$commish_corner,
    vapply(result$awards %||% list(), function(a) a$blurb %||% "", character(1))),
  collapse = " "
)
claims <- as.numeric(unlist(regmatches(
  out_txt, gregexpr("[0-9]+\\.?[0-9]*(?=%)", out_txt, perl = TRUE)
)))
supported <- vapply(claims, function(p) {
  any(abs(data_nums * 100 - p) <= 1.5 | abs(data_nums - p) <= 1.5, na.rm = TRUE)
}, logical(1))
if (any(!supported)) {
  flags <- c(flags, paste("Percentages not found in the data, verify:",
                          paste0(claims[!supported], "%", collapse = ", ")))
}

# Write outputs -------------------------------------------------------------

out <- list(
  season = latest$season,
  week = week,
  mode = mode,
  model = model,
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  bullets = unlist(result$bullets),
  commish_corner = result$commish_corner,
  review_flags = flags
)
if (!is.null(result$awards)) out$awards <- result$awards
write_json(out, out_json, auto_unbox = TRUE, pretty = TRUE)

md <- c(
  sprintf("## Week %d draft (%s mode)", week, mode), "",
  paste0("- ", out$bullets),
  paste0("- <u><strong>Commish Corner</strong></u> - ", out$commish_corner)
)
if (!is.null(out$awards)) {
  md <- c(md, "", "### Awards",
          vapply(out$awards, function(a) {
            sprintf("- **%s** - %s: %s", a$team, a$award, a$blurb)
          }, character(1)))
}
if (length(flags)) md <- c(md, "", "### Review flags", paste0("- ", flags))
md <- c(md, "", "_Merge to publish. Edit commentary.json in this PR first if anything needs fixing._")

writeLines(md, file.path(week_dir, "commentary.md"))
writeLines(md, here::here("pr_body.md"))  # used as the PR description; not committed
cat(md, sep = "\n")
