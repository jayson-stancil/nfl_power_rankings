# =============================================================================
# NFL Power Rankings (Glicko-2) -- independent weekly pipeline
#
# nflverse schedule -> Glicko-2 ratings seeded by a preseason Borda
# consensus -> two CSVs committed to this repo:
#   data/current.csv  -- latest week's full standings table (one row per
#                         team: rank, rating, record, week-over-week change)
#   data/history.csv  -- long-format rating history (Week, Team, ABBREV,
#                         Rating), one row per team per week since Week 2 --
#                         every league app's "Rating Trajectory" view reads
#                         this one file for its line chart.
#   data/teams.csv    -- ABBREV -> full team name + ESPN logo URL, used by
#                         the apps' gt table (kept out of current.csv so the
#                         apps don't need nflreadr just to show a logo).
#   data/meta.csv     -- season, latest_week, week_label, run_date.
#
# This repo has no Shiny app of its own -- the three fantasy-league apps
# (adg_dynasty_league_power_rankings, sleeper-power-rankings,
# corbin_dynasty_league_power_rankings) each read these four CSVs straight
# off raw.githubusercontent.com, the same way they already read their own
# weekly data. Nothing here is league-specific.
#
# Usage:  Rscript R/nfl_power_rankings.R            (all completed games)
#         Rscript R/nfl_power_rankings.R 3          (only games through week 3)
#
# Volatility is standard Glicko-2: every team starts at 0.06 and the algorithm
# updates it from wins/losses only. Score margins are never used.
#
# Author: Jayson Stancil | 2026-10-19
# =============================================================================

SEASON   <- 2026
DATA_DIR <- "data"
PRESEASON_FILE <- file.path(DATA_DIR, "preseason.csv")

required_pkgs <- c("PlayerRatings", "nflreadr")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                       logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("Missing packages: ", paste(missing_pkgs, collapse = ", "))
}

args <- commandArgs(trailingOnly = TRUE)
through_week <- if (length(args)) as.integer(args[1]) else Inf

# --------------------------- PRESEASON PRIOR ---------------------------------

# Rating = 1500 + z(Borda) * 100: mean exactly 1500, top vs bottom team
# starts near an 85% expected win probability.
pre <- read.csv(PRESEASON_FILE, stringsAsFactors = FALSE, check.names = FALSE)
pre <- pre[, c("Team", "ABBREV", "BordaScore", "BordaRank")]
initial_ratings <- data.frame(
  Player     = pre$ABBREV,
  Rating     = 1500 + as.numeric(scale(pre$BordaScore)) * 100,
  Deviation  = 200,
  Volatility = 0.06
)

# --------------------------- GAMES -------------------------------------------

sched <- as.data.frame(nflreadr::load_schedules(SEASON))
# Only weeks where every scheduled game is final (skips a lone Thursday game)
done_weeks <- tapply(!is.na(sched$result), sched$week, all)
last_done  <- suppressWarnings(max(as.integer(names(done_weeks)[done_weeks])))
through_week <- min(through_week, last_done)
games <- sched[!is.na(sched$result) & sched$week <= through_week,
               c("week", "game_type", "away_team", "away_score",
                 "home_team", "home_score")]
if (!nrow(games)) stop("No completed ", SEASON, " games found.")
games <- games[order(games$week), ]
games$result <- ifelse(games$home_score > games$away_score, 1,
                       ifelse(games$home_score < games$away_score, 0, 0.5))
unknown <- setdiff(c(games$home_team, games$away_team), pre$ABBREV)
if (length(unknown)) stop("Team codes not in preseason file: ",
                          paste(unknown, collapse = ", "))

latest_week <- max(games$week)
week_label  <- if (any(games$game_type == "SB")) "Final" else
               paste("Week", latest_week + 1)

run_glicko <- function(g) {
  if (!nrow(g)) {
    out <- initial_ratings
    out[c("Games", "Win", "Draw", "Loss")] <- 0
    return(out)
  }
  g2 <- PlayerRatings::glicko2(
    data.frame(week = g$week, home = g$home_team, away = g$away_team,
               result = g$result),
    status = initial_ratings)
  as.data.frame(g2$ratings)
}

now  <- run_glicko(games)
prev <- run_glicko(games[games$week < latest_week, ])

# --------------------------- CURRENT-WEEK TABLE ------------------------------

now  <- now[order(-now$Rating), ]
prev <- prev[order(-prev$Rating), ]
now$Rank      <- seq_len(nrow(now))
prev_rank     <- match(now$Player, prev$Player)
prev_rating   <- prev$Rating[prev_rank]

tbl <- data.frame(
  Rank            = now$Rank,
  Team            = pre$Team[match(now$Player, pre$ABBREV)],
  ABBREV          = now$Player,
  Rating          = round(now$Rating, 1),
  Deviation       = round(now$Deviation, 1),
  Volatility      = round(now$Volatility, 4),
  Record          = paste0(now$Win, "-", now$Loss,
                           ifelse(now$Draw > 0, paste0("-", now$Draw), "")),
  RankChange      = prev_rank - now$Rank,
  RatingChange    = round(now$Rating - prev_rating, 1),
  PreseasonRank   = pre$BordaRank[match(now$Player, pre$ABBREV)]
)

if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)

current_out <- cbind(Week = latest_week, WeekLabel = paste("Week", latest_week),
                     tbl)
write.csv(current_out, file.path(DATA_DIR, "current.csv"), row.names = FALSE)
message("Current-week table written: data/current.csv (", week_label,
        " heading in, ratings through Week ", latest_week, ")")

# --------------------------- RATING HISTORY ----------------------------------

# Long format, one row per team per week, appended to (not overwritten by)
# every run so the apps' trajectory chart has the full season to draw. Each
# run recomputes every week from scratch (Glicko-2 is deterministic given
# the same games), then replaces any existing rows for weeks it just
# recomputed -- so a re-run or a late game correction never duplicates or
# orphans a week's rows.
history_file <- file.path(DATA_DIR, "history.csv")
history_rows <- do.call(rbind, lapply(sort(unique(games$week)), function(w) {
  r <- run_glicko(games[games$week <= w, ])
  data.frame(Week = w, WeekLabel = paste("Week", w),
             Team = pre$Team[match(r$Player, pre$ABBREV)],
             ABBREV = r$Player, Rating = round(r$Rating, 1))
}))
# Week 1 (no games played, pure preseason prior) is intentionally excluded --
# every team starts at the same relative spread from the Borda prior, so a
# "Week 1" point on the trajectory chart would just be restating the
# preseason rank, not a result.
history_rows <- history_rows[history_rows$Week > 1, ]

existing_history <- if (file.exists(history_file)) {
  read.csv(history_file, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  history_rows[0, ]
}
existing_history <- existing_history[!(existing_history$Week %in% history_rows$Week), ]
history_out <- rbind(existing_history, history_rows)
history_out <- history_out[order(history_out$Week, history_out$Team), ]
write.csv(history_out, history_file, row.names = FALSE)
message("Rating history written: ", history_file, " (", nrow(history_out),
        " team-weeks)")

# --------------------------- TEAM REFERENCE ----------------------------------

teams <- as.data.frame(nflreadr::load_teams())
teams_out <- data.frame(
  ABBREV = pre$ABBREV,
  Team   = pre$Team,
  Logo   = teams$team_logo_espn[match(pre$ABBREV, teams$team_abbr)]
)
write.csv(teams_out, file.path(DATA_DIR, "teams.csv"), row.names = FALSE)
message("Team reference written: data/teams.csv")

# --------------------------- META --------------------------------------------

writeLines(c(paste0("season,", SEASON),
            paste0("latest_week,", latest_week),
            paste0("week_label,", week_label),
            paste0("run_date,", as.character(Sys.Date()))),
          file.path(DATA_DIR, "meta.csv"))

message("Done. ", week_label, " ", SEASON, " NFL power rankings complete.")
