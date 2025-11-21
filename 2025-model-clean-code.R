library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(purrr)
library(httr)
library(jsonlite)
library(ggplot2)
library(readr)
library(DT)
library(scales)
library(lubridate)
library(ggimage) # for displaying logos on scatter plots

###########################################################################
#                              Helper functions                           #
###########################################################################

# Set your API key
Sys.setenv(CFBD_API_KEY = "iF6R6JQZgMSDa53x4XnUsbPQ8yh+SNav03/N6yF8sWK8zSls4DAJD3REERc09YIf")

# Define %||% early (used below)
`%||%` <- function(x, y) if (is.null(x)) y else x

# Retrieve the CFBD API token from the environment. Uses CFBD_API_KEY.
cfbd_token <- function() {
  tok <- Sys.getenv("CFBD_API_KEY", unset = NA_character_)
  if (is.na(tok) || tok == "") tok <- getOption("CFBD_API_KEY", default = NA_character_)
  if (is.na(tok) || tok == "") {
    stop(
      "No CFBD token found. Set it with:\n",
      'Sys.setenv(CFBD_API_KEY = "<your-token>")\n',
      "Or add CFBD_API_KEY to your .Renviron."
    )
  }
  tok
}

# Generic helper to perform GET requests against the CFBD REST API.
cfbd_get <- function(path, query = list(), base = "https://api.collegefootballdata.com") {
  url   <- paste0(base, path)
  token <- cfbd_token()
  resp <- httr::RETRY(
    verb  = "GET",
    url   = url,
    query = query,
    httr::add_headers(Authorization = paste("Bearer", token)),
    times = 3, pause_base = 1, pause_cap = 5
  )
  httr::stop_for_status(resp)
  jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
}

# Collision-proof HTTP helper (unique name so we never hit another cfbd_get)
cfbd_http_get <- function(path, query = list(), base = "https://api.collegefootballdata.com") {
  url   <- paste0(base, path)
  token <- cfbd_token()
  resp <- httr::RETRY(
    "GET", url,
    query = query,
    httr::add_headers(Authorization = paste("Bearer", token)),
    times = 3, pause_base = 1, pause_cap = 5
  )
  httr::stop_for_status(resp)
  jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
}

# Fetch the full schedule for a given year. Returns a tibble per game.
get_cfbd_schedule <- function(year) {
  data <- cfbd_get("/games", query = list(year = year, seasonType = "both"))
  tibble::as_tibble(data) |>
    dplyr::rename(
      game_id         = id,
      season          = season,
      week            = week,
      season_type     = seasonType,
      start_date      = startDate,
      home_team       = homeTeam,
      home_points     = homePoints,
      home_conference = homeConference,
      away_team       = awayTeam,
      away_points     = awayPoints,
      away_conference = awayConference,
      neutral_site    = neutralSite
    ) |>
    dplyr::mutate(start_date = lubridate::ymd_hms(start_date, quiet = TRUE))
}

# Retrieve Bill Connelly's SP+ ratings for a specific year.
get_sp_plus <- function(year) {
  data <- cfbd_get("/ratings/sp", query = list(year = year))
  tibble::as_tibble(data) %>%
    dplyr::transmute(
      year,
      team,
      conference,
      sp_overall = rating,
      sp_ranking = ranking,
      sp_offense_rating = offense.rating,
      sp_offense_ranking = offense.ranking,
      sp_defense_rating = defense.rating,
      sp_defense_ranking = defense.ranking,
    )
}

get_success_rate <- function(year) {
  tb <- tibble::as_tibble(
    cfbd_get("/stats/season/advanced", query = list(year = year))
  )

  # If jsonlite flattened, names will have dots. Normalize to underscores:
  names(tb) <- gsub("\\.", "_", names(tb))

  # If we already have flattened columns (common with flatten=TRUE), use them directly
  flattened_cols <- c(
    "offense_successRate","offense_ppa","offense_explosiveness",
    "defense_successRate","defense_ppa","defense_explosiveness",
    "offense_rushingPlays_successRate","offense_rushingPlays_ppa","offense_rushingPlays_explosiveness",
    "offense_passingPlays_successRate","offense_passingPlays_ppa","offense_passingPlays_explosiveness",
    "defense_rushingPlays_successRate","defense_rushingPlays_ppa","defense_rushingPlays_explosiveness",
    "defense_passingPlays_successRate","defense_passingPlays_ppa","defense_passingPlays_explosiveness"
  )

  if (all(c("team","conference") %in% names(tb))) {
    # ensure expected columns exist
    for (nm in flattened_cols) if (!nm %in% names(tb)) tb[[nm]] <- NA_real_

    return(
      tb %>%
        dplyr::transmute(
          season = year,
          team,
          conference,
          offense_success_rate       = as.numeric(.data$offense_successRate),
          offense_ppa                = as.numeric(.data$offense_ppa),
          offense_explosiveness      = as.numeric(.data$offense_explosiveness),
          offense_rush_success       = as.numeric(.data$offense_rushingPlays_successRate),
          offense_rush_ppa           = as.numeric(.data$offense_rushingPlays_ppa),
          offense_rush_explosiveness = as.numeric(.data$offense_rushingPlays_explosiveness),
          offense_pass_success       = as.numeric(.data$offense_passingPlays_successRate),
          offense_pass_ppa           = as.numeric(.data$offense_passingPlays_ppa),
          offense_pass_explosiveness = as.numeric(.data$offense_passingPlays_explosiveness),

          defense_success_rate       = as.numeric(.data$defense_successRate),
          defense_ppa                = as.numeric(.data$defense_ppa),
          defense_explosiveness      = as.numeric(.data$defense_explosiveness),
          defense_rush_success       = as.numeric(.data$defense_rushingPlays_successRate),
          defense_rush_ppa           = as.numeric(.data$defense_rushingPlays_ppa),
          defense_rush_explosiveness = as.numeric(.data$defense_rushingPlays_explosiveness),
          defense_pass_success       = as.numeric(.data$defense_passingPlays_successRate),
          defense_pass_ppa           = as.numeric(.data$defense_passingPlays_ppa),
          defense_pass_explosiveness = as.numeric(.data$defense_passingPlays_explosiveness)
        )
    )
  }

  # Fallback: if payload is nested (rare with flatten=TRUE), pull via map_dbl
  grab_num <- function(x, path) {
    val <- tryCatch(Reduce(function(acc, nm) acc[[nm]], path, init = x), error = function(...) NULL)
    if (is.null(val)) NA_real_ else suppressWarnings(as.numeric(val))
  }

  if ("offense" %in% names(tb) && "defense" %in% names(tb)) {
    return(
      tb %>%
        dplyr::mutate(
          offense_success_rate       = purrr::map_dbl(offense, ~ grab_num(.x, c("successRate"))),
          offense_ppa                = purrr::map_dbl(offense, ~ grab_num(.x, c("ppa"))),
          offense_explosiveness      = purrr::map_dbl(offense, ~ grab_num(.x, c("explosiveness"))),
          offense_rush_success       = purrr::map_dbl(offense, ~ grab_num(.x, c("rushingPlays","successRate"))),
          offense_rush_ppa           = purrr::map_dbl(offense, ~ grab_num(.x, c("rushingPlays","ppa"))),
          offense_rush_explosiveness = purrr::map_dbl(offense, ~ grab_num(.x, c("rushingPlays","explosiveness"))),
          offense_pass_success       = purrr::map_dbl(offense, ~ grab_num(.x, c("passingPlays","successRate"))),
          offense_pass_ppa           = purrr::map_dbl(offense, ~ grab_num(.x, c("passingPlays","ppa"))),
          offense_pass_explosiveness = purrr::map_dbl(offense, ~ grab_num(.x, c("passingPlays","explosiveness"))),

          defense_success_rate       = purrr::map_dbl(defense, ~ grab_num(.x, c("successRate"))),
          defense_ppa                = purrr::map_dbl(defense, ~ grab_num(.x, c("ppa"))),
          defense_explosiveness      = purrr::map_dbl(defense, ~ grab_num(.x, c("explosiveness"))),
          defense_rush_success       = purrr::map_dbl(defense, ~ grab_num(.x, c("rushingPlays","successRate"))),
          defense_rush_ppa           = purrr::map_dbl(defense, ~ grab_num(.x, c("rushingPlays","ppa"))),
          defense_rush_explosiveness = purrr::map_dbl(defense, ~ grab_num(.x, c("rushingPlays","explosiveness"))),
          defense_pass_success       = purrr::map_dbl(defense, ~ grab_num(.x, c("passingPlays","successRate"))),
          defense_pass_ppa           = purrr::map_dbl(defense, ~ grab_num(.x, c("passingPlays","ppa"))),
          defense_pass_explosiveness = purrr::map_dbl(defense, ~ grab_num(.x, c("passingPlays","explosiveness")))
        ) %>%
        dplyr::transmute(
          season = year,
          team,
          conference,
          dplyr::starts_with("offense_"),
          dplyr::starts_with("defense_")
        )
    )
  }

  # If the shape is unexpected, return a minimal shell to avoid crashes
  tibble::tibble(
    season = integer(0), team = character(0), conference = character(0),
    offense_success_rate = numeric(0), offense_ppa = numeric(0), offense_explosiveness = numeric(0),
    defense_success_rate = numeric(0), defense_ppa = numeric(0), defense_explosiveness = numeric(0)
  )
}

# Retrieve team-level talent ratings.
get_team_talent <- function(year) {
  data <- cfbd_get("/talent", query = list(year = year))
  tibble::as_tibble(data) %>%
    dplyr::transmute(
      season = year,
      team,
      talent_score = .data$talent
    )
}

# Fetch returning production metrics for a given year (uses collision-proof helper).
get_returning_production <- function(year) {
  data <- cfbd_http_get("/player/returning", query = list(year = year))
  tibble::as_tibble(data) %>%
    dplyr::transmute(
      season                  = year,
      team                    = .data$team,
      conference              = .data$conference,
      total_ppa               = purrr::map_dbl(.data$totalPPA,               ~ .x %||% NA_real_),
      total_passing_ppa       = purrr::map_dbl(.data$totalPassingPPA,        ~ .x %||% NA_real_),
      total_receiving_ppa     = purrr::map_dbl(.data$totalReceivingPPA,      ~ .x %||% NA_real_),
      total_rushing_ppa       = purrr::map_dbl(.data$totalRushingPPA,        ~ .x %||% NA_real_),
      percent_ppa             = purrr::map_dbl(.data$percentPPA,             ~ .x %||% NA_real_),
      percent_passing_ppa     = purrr::map_dbl(.data$percentPassingPPA,      ~ .x %||% NA_real_),
      percent_receiving_ppa   = purrr::map_dbl(.data$percentReceivingPPA,    ~ .x %||% NA_real_),
      percent_rushing_ppa     = purrr::map_dbl(.data$percentRushingPPA,      ~ .x %||% NA_real_),
      usage                   = purrr::map_dbl(.data$usage,                  ~ .x %||% NA_real_),
      passing_usage           = purrr::map_dbl(.data$passingUsage,           ~ .x %||% NA_real_),
      receiving_usage         = purrr::map_dbl(.data$receivingUsage,         ~ .x %||% NA_real_),
      rushing_usage           = purrr::map_dbl(.data$rushingUsage,           ~ .x %||% NA_real_)
    )
}

###########################################################################
#                           Data Preparation                               #
###########################################################################

# Build historical datasets for modelling.
start_train_year <- 2015
end_train_year   <- 2025
predict_year     <- 2025
stats_year       <- 2025  # last completed season used for baseline statistics

message("Fetching schedules from ", start_train_year, " to ", end_train_year)
schedule_hist <- purrr::map_dfr(start_train_year:end_train_year, function(y) {
  tryCatch(get_cfbd_schedule(y), error = function(e) {
    warning("Schedule API failed for year ", y, ": ", e$message)
    NULL
  })
})

get_returning_production <- function(year, team = NULL, conference = NULL, cfbd_token = NULL) {
  # IMPORTANT: use params=, and the endpoint /player/returning
  prms <- list(year = year)
  if (!is.null(team))       prms$team       <- team
  if (!is.null(conference)) prms$conference <- conference

  cfbd_get("/player/returning", params = prms, token = cfbd_token) |>
    dplyr::mutate(season = year)
}

returning_hist <- purrr::map_dfr(start_train_year:(stats_year), function(y) {
   tryCatch(
    get_returning_production(y),
   error = function(e) {
      warning("Returning production API failed for year ", y, ": ", e$message)
       tibble::tibble(
         season = integer(0), team = character(0), conference = character(0),
         total_ppa = numeric(0), total_passing_ppa = numeric(0),
        total_receiving_ppa = numeric(0), total_rushing_ppa = numeric(0),
        percent_ppa = numeric(0), percent_passing_ppa = numeric(0),
       percent_receiving_ppa = numeric(0), percent_rushing_ppa = numeric(0),
       usage = numeric(0), passing_usage = numeric(0),
       receiving_usage = numeric(0), rushing_usage = numeric(0)
     )
    }
  )
})

message("Fetching SP+ ratings")
sp_plus_hist <- purrr::map_dfr(start_train_year:stats_year, function(y) {
  tryCatch(get_sp_plus(y), error = function(e) {
    warning("SP+ API failed for year ", y, ": ", e$message)
    NULL
  })
})

message("Fetching success rates")
success_hist <- purrr::map_dfr(start_train_year:stats_year, function(y) {
  tryCatch(get_success_rate(y), error = function(e) {
    warning("Success rates API failed for year ", y, ": ", e$message)
    NULL
  })
})

message("Fetching team talent")
talent_hist <- purrr::map_dfr(start_train_year:stats_year, function(y) {
  tryCatch(get_team_talent(y), error = function(e) {
    warning("Talent API failed for year ", y, ": ", e$message)
    NULL
  })
})

get_adv_stats <- function(year) {
  endpoint <- "https://api.collegefootballdata.com/stats/season"

  res <- httr::GET(
    url = endpoint,
    query = list(year = year, category = "advanced"),
    httr::add_headers(Authorization = paste("Bearer", cfbd_token()))
  )

  if (httr::http_type(res) != "application/json") return(NULL)

  data <- httr::content(res, as = "parsed", simplifyVector = TRUE)
  if (length(data) == 0 || is.null(data)) return(NULL)

  flat_df <- tibble::as_tibble(data)

  return(flat_df)
}

# Advanced Stats All
year <- 2014:2025
adv_stats_all <- map_dfr(year, ~get_adv_stats(.x), .progress = TRUE)


adv_stats_all_wide <- adv_stats_all %>%
  tidyr::pivot_wider(
    names_from = statName,
    values_from = statValue
  )

# -------------------------------------------------------------------------
# Stop-the-Bleed Index (SBI) computation
#

drives14 <- read.csv("/home/aaronhef/Downloads/2014-drives.csv")
drives15 <- read.csv("/home/aaronhef/Downloads/2015-drives.csv")
drives16 <- read.csv("/home/aaronhef/Downloads/2016-drives.csv")
drives17 <- read.csv("/home/aaronhef/Downloads/2017-drives.csv")
drives18 <- read.csv("/home/aaronhef/Downloads/2018-drives.csv")
drives19 <- read.csv("/home/aaronhef/Downloads/2019-drives.csv")
drives20 <- read.csv("/home/aaronhef/Downloads/2020-drives.csv")
drives21 <- read.csv("/home/aaronhef/Downloads/2021-drives.csv")
drives22 <- read.csv("/home/aaronhef/Downloads/2022-drives.csv")
drives23 <- read.csv("/home/aaronhef/Downloads/2023-drives.csv")
drives24 <- read.csv("/home/aaronhef/Downloads/2024-drives.csv")
drives25 <- read.csv("/home/aaronhef/Downloads/drives-12.csv")

drives14$season <- 2014
drives15$season <- 2015
drives16$season <- 2016
drives17$season <- 2017
drives18$season <- 2018
drives19$season <- 2019
drives20$season <- 2020
drives21$season <- 2021
drives22$season <- 2022
drives23$season <- 2023
drives24$season <- 2024
drives25$season <- 2025

team_drives_all_years <- bind_rows(
  drives14, drives15, drives16, drives17, drives18,
  drives19, drives20, drives21, drives22, drives23, drives24, drives25
)
team_drives_all <- team_drives_all_years
team_drives_all <- team_drives_all %>%
  mutate(Scoring = Scoring == "true")

sbi_raw <- team_drives_all %>%
  arrange(GameId, DriveNumber) %>%
  group_by(GameId, season) %>%
  mutate(
    prev_drive_team    = lag(Offense),
    prev_drive_scoring = lag(Scoring),
    prev_drive_season  = lag(season)
  ) %>%
  ungroup() %>%
  filter(
    prev_drive_scoring == TRUE,
    prev_drive_team != Offense
  ) %>%
  transmute(
    team = Offense,
    season = season,
    sbi_success = Scoring
  ) %>%
  filter(!is.na(team), !is.na(season))

sbi_by_team_year <- sbi_raw %>%
  group_by(team, season) %>%
  summarise(
    sbi_attempts  = n(),
    sbi_successes = sum(sbi_success, na.rm = TRUE),
    stop_the_bleed_index = sbi_successes / sbi_attempts,
    .groups = "drop"
  )

make_team_side <- function(df, prefix) {
  df %>% rename_with(~ paste0(prefix, "_", .), -c(season, team)) %>%
    rename(season = season, team = team)
}

sp_plus_hist$season <- sp_plus_hist$year

# Prepare home and away copies of each metric table for the training period.
home_sp_plus    <- make_team_side(sp_plus_hist,    "home")
away_sp_plus    <- make_team_side(sp_plus_hist,    "away")
home_success    <- make_team_side(success_hist,    "home")
away_success    <- make_team_side(success_hist,    "away")
home_talent     <- make_team_side(talent_hist,     "home")
away_talent     <- make_team_side(talent_hist,     "away")
home_returning  <- make_team_side(returning_hist %>% filter(.data$season <= stats_year), "home")
away_returning  <- make_team_side(returning_hist %>% filter(.data$season <= stats_year), "away")

# Prepare home and away copies of the stop‑the‑bleed index for the training period.
home_sbi <- make_team_side(sbi_by_team_year %>% filter(.data$season <= stats_year), "home")
away_sbi <- make_team_side(sbi_by_team_year %>% filter(.data$season <= stats_year), "away")

# Merge home and away features onto the historical schedule.
schedule_modeling <- schedule_hist %>%
  filter(.data$season >= (start_train_year + 1), .data$season <= stats_year) %>%
  left_join(home_sp_plus,   by = c("season", "home_team" = "team")) %>%
  left_join(away_sp_plus,   by = c("season", "away_team" = "team")) %>%
  left_join(home_success,   by = c("season", "home_team" = "team")) %>%
  left_join(away_success,   by = c("season", "away_team" = "team")) %>%
  left_join(home_talent,    by = c("season", "home_team" = "team")) %>%
  left_join(away_talent,    by = c("season", "away_team" = "team")) %>%
  left_join(home_returning, by = c("season", "home_team" = "team")) %>%
  left_join(away_returning, by = c("season", "away_team" = "team")) %>%
  left_join(home_sbi, by = c("season", "home_team" = "team")) %>%
  left_join(away_sbi, by = c("season", "away_team" = "team"))

# Compute feature differences between home and away teams.  Use as.numeric
# to force numeric types; missing values remain NA.
schedule_modeling_diff <- schedule_modeling %>%
  mutate(
    sp_overall_diff       = as.numeric(home_sp_overall) - as.numeric(away_sp_overall),
    sp_offense_diff       = as.numeric(home_sp_offense_rating) - as.numeric(away_sp_offense_rating),
    sp_defense_diff       = as.numeric(home_sp_defense_rating) - as.numeric(away_sp_defense_rating),
    success_off_diff      = as.numeric(home_offense_success_rate) - as.numeric(away_offense_success_rate),
    success_def_diff      = as.numeric(home_defense_success_rate) - as.numeric(away_defense_success_rate),
    talent_diff           = as.numeric(home_talent_score) - as.numeric(away_talent_score),
    return_total_ppa_diff   = as.numeric(home_total_ppa) - as.numeric(away_total_ppa),
    return_off_ppa_diff     = (as.numeric(home_total_passing_ppa) + as.numeric(home_total_receiving_ppa)) -
                              (as.numeric(away_total_passing_ppa) + as.numeric(away_total_receiving_ppa)),
    return_rush_ppa_diff    = as.numeric(home_total_rushing_ppa) - as.numeric(away_total_rushing_ppa),
    return_percent_ppa_diff = as.numeric(home_percent_ppa) - as.numeric(away_percent_ppa),
    return_usage_diff       = as.numeric(home_usage) - as.numeric(away_usage)
    ,
    # Stop‑the‑bleed index differential
    sbi_index_diff          = as.numeric(home_stop_the_bleed_index) - as.numeric(away_stop_the_bleed_index),
    sbi_attempts_diff       = as.numeric(home_sbi_attempts) - as.numeric(away_sbi_attempts)
  ) %>%
  mutate(
    home_win = as.integer(home_points > away_points),
    margin_of_victory = home_points - away_points
  ) %>%
  select(
    game_id, season, week, home_team, away_team,
    home_win, margin_of_victory,
    sp_overall_diff, sp_offense_diff, sp_defense_diff,
    success_off_diff, success_def_diff,
    talent_diff,
    return_total_ppa_diff, return_off_ppa_diff, return_rush_ppa_diff,
    return_percent_ppa_diff, return_usage_diff,
    sbi_index_diff, sbi_attempts_diff
  )

schedule_modeling_diff_2 <- schedule_modeling_diff

training_data <- schedule_modeling_diff %>%
  filter(season < "2025") %>%
  drop_na()

###########################################################################
#                              Model Training                             #
###########################################################################

# CFBONE: a logistic regression focusing on overall strength, talent and
# returning production.  Predicts probability that the home team wins.
cfbone_model <- glm(
  home_win ~ sp_overall_diff + talent_diff + return_percent_ppa_diff,
  data = training_data,
  family = binomial(link = "logit")
)

# Blue: a logistic regression emphasising offensive and defensive splits and
# success rates.
blue_model <- glm(
  home_win ~ sp_offense_diff + sp_defense_diff + success_off_diff + success_def_diff,
  data = training_data,
  family = binomial(link = "logit")
)

# Gryphon: a logistic regression that folds in special‑teams proxies – here
# represented by returning production splits.  Should the stop‑the‑bleed
# index or other special‑teams metric be available, add it here.  For now
# we include offensive and defensive returning production along with SP+
# overall.
gryphon_model <- glm(
  home_win ~ sp_overall_diff + return_off_ppa_diff + return_rush_ppa_diff + talent_diff +
    sbi_index_diff,
  data = training_data,
  family = binomial(link = "logit")
)

# Blue margin model: a linear regression predicting margin of victory using
# the same predictors as Blue.  Used to estimate the expected spread.
blue_spread_model <- lm(
  margin_of_victory ~ sp_offense_diff + sp_defense_diff + success_off_diff + success_def_diff,
  data = training_data
)

# Gryphon margin model: linear regression including returning splits.
gryphon_spread_model <- lm(
  margin_of_victory ~ sp_overall_diff + return_off_ppa_diff + return_rush_ppa_diff + talent_diff +
    sbi_index_diff,
  data = training_data
)

# "Wins of Gryphon" calibration:  a simple logistic regression that
# estimates win probability using only the overall SP+ difference.  The
# resulting model transforms a difference in power ratings into a win
# probability; this echoes an older heuristic used in previous versions
# of the modelling workflow.
gryphon_power_glm <- glm(
  home_win ~ sp_overall_diff,
  data = training_data,
  family = binomial(link = "logit")
)

# Precompute a calibration curve for the "Wins of Gryphon" heuristic.  This
# tibble maps differences in SP+ overall rating to the corresponding
# probability of a home victory according to the simple logistic model.  It
# is not used directly by the application but retained for reference and
# potential future visualisations.
gryphon_power_curve <- tibble::tibble(
  sp_overall_diff = seq(-40, 40, by = 0.5)
) %>%
  mutate(win_prob = as.numeric(predict(gryphon_power_glm,
                                       newdata = ., type = "response")))

# Estimate the typical prediction error (standard deviation of residuals) to
# generate spread distributions later.  We take the mean of the two models'
# residual standard deviations as a rough approximation.
sigma_blue    <- sd(training_data$margin_of_victory - predict(blue_spread_model, newdata = training_data))
sigma_gryphon <- sd(training_data$margin_of_victory - predict(gryphon_spread_model, newdata = training_data))
sigma_spread  <- mean(c(sigma_blue, sigma_gryphon), na.rm = TRUE)

###########################################################################
#                     Projecting the 2025 Season                          #
###########################################################################

schedule_2025_diff <- schedule_modeling_diff %>% filter(season == 2025)

predictions_2025 <- schedule_2025_diff %>%
  tidyr::drop_na(sp_overall_diff, sp_offense_diff, sp_defense_diff,
                 success_off_diff, success_def_diff, talent_diff,
                 return_total_ppa_diff, return_off_ppa_diff,
                 return_rush_ppa_diff, return_percent_ppa_diff,
                 sbi_index_diff) %>%
  mutate(
    prob_cfbone  = as.numeric(predict(cfbone_model, newdata = ., type = "response")),
    prob_blue    = as.numeric(predict(blue_model,   newdata = ., type = "response")),
    prob_gryphon = as.numeric(predict(gryphon_model,newdata = ., type = "response")),
    # A simple power-based probability using only the SP+ overall difference.
    prob_gryphon_power = as.numeric(predict(gryphon_power_glm, newdata = ., type = "response")),
    win_prob     = (prob_cfbone + prob_blue + prob_gryphon) / 3,
    spread_blue  = as.numeric(predict(blue_spread_model,    newdata = .)),
    spread_gryph = as.numeric(predict(gryphon_spread_model, newdata = .)),
    spread_pred  = (spread_blue + spread_gryph) / 2,
    predicted_winner = ifelse(win_prob >= 0.5, home_team, away_team)

# 3) Build the site
source("build_hefml_football_site 6.R")

build_hefml_football_site(
  predictions_2025,
  template_file = "hefml_1107.html",
  output_file   = "hefml_football.html",
  team_logos    = team_logos  # or NULL if you want to fall back to text points
)
  )
