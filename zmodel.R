# Load libraries
library(shiny)
library(bslib)
library(DT)
library(data.table)
library(plotly)
library(catboost)
library(quantregForest)
library(caret)
library(httr)
library(jsonlite)
library(zoo)

# API Key & Base URL
api_key <- "iF6R6JQZgMSDa53x4XnUsbPQ8yh+SNav03/N6yF8sWK8zSls4DAJD3REERc09YIf"
base_url <- "https://api.collegebasketballdata.com/"

# Fetch data functions with error handling
fetch_games <- function(season, status = "final") {
  url <- paste0(base_url, "games?season=", season, "&status=", status)
  tryCatch({
    response <- GET(url, add_headers(`Authorization` = paste("Bearer", api_key)))
    if (status_code(response) != 200) {
      stop("API returned status: ", status_code(response))
    }
    data <- fromJSON(content(response, "text"), flatten = TRUE)
    return(data)
  }, error = function(e) {
    message("API fetch failed for games season ", season, ": ", e$message)
    return(NULL)
  })
}

fetch_team_stats <- function(season) {
  url <- paste0(base_url, "stats/team/season?season=", season)
  tryCatch({
    response <- GET(url, add_headers(`Authorization` = paste("Bearer", api_key)))
    if (status_code(response) != 200) {
      stop("API returned status: ", status_code(response))
    }
    data <- fromJSON(content(response, "text"), flatten = TRUE)
    return(data)
  }, error = function(e) {
    message("API fetch failed for team stats season ", season, ": ", e$message)
    return(NULL)
  })
}

fetch_game_data_multiple <- function(seasons) {
  data_list <- lapply(seasons, fetch_games)
  dt <- rbindlist(data_list)
  if ("homePoints" %in% names(dt) && "awayPoints" %in% names(dt)) {
    dt[!is.na(homePoints) & !is.na(awayPoints), 
       .(season, startDate, homeTeam, awayTeam, homePoints, awayPoints)]
  } else {
    dt[, .(season, startDate, homeTeam, awayTeam)]
  }
}

fetch_team_stats_multiple <- function(seasons) {
  data_list <- lapply(seasons, fetch_team_stats)
  dt <- rbindlist(data_list)
  dt[, .(season, team, pace, teamStats.trueShooting, teamStats.fourFactors.effectiveFieldGoalPct,
         teamStats.fourFactors.turnoverRatio, teamStats.fourFactors.offensiveReboundPct,
         teamStats.fourFactors.freeThrowRate, opponentStats.trueShooting, teamStats.points.total,
         teamStats.points.inPaint, teamStats.points.offTurnovers, teamStats.points.fastBreak)]
}

# Load and process data
seasons <- 2018:2026
games_data <- fetch_game_data_multiple(seasons)
team_stats <- fetch_team_stats_multiple(seasons)
future_games <- fetch_games(2026, status = "scheduled")

# Calculate final Elo ratings from past games
calculate_final_elo <- function(games_data) {
  setDT(games_data)
  games_data <- games_data[order(startDate)]
  teams <- unique(c(games_data$homeTeam, games_data$awayTeam))
  elo <- rep(1500, length(teams))
  names(elo) <- teams
  for (i in 1:nrow(games_data)) {
    home <- games_data$homeTeam[i]
    away <- games_data$awayTeam[i]
    elo_h <- elo[home]
    elo_a <- elo[away]
    exp_h <- 1 / (1 + 10^((elo_a - elo_h - 100) / 400))
    if ("homePoints" %in% names(games_data) && "awayPoints" %in% names(games_data) &&
        !is.na(games_data$homePoints[i]) && !is.na(games_data$awayPoints[i])) {
      result <- ifelse(games_data$homePoints[i] > games_data$awayPoints[i], 1, 0)
      elo[home] <- elo_h + 20 * (result - exp_h)
      elo[away] <- elo_a + 20 * ((1 - result) - (1 - exp_h))
    }
  }
  return(elo)
}

# Update Elo ratings for past games
update_elo <- function(data, k = 20) {
  setDT(data)
  data <- data[order(startDate)]
  teams <- unique(c(data$homeTeam, data$awayTeam))
  elo <- rep(1500, length(teams))
  names(elo) <- teams
  has_scores <- "homePoints" %in% names(data) && "awayPoints" %in% names(data)
  for (i in 1:nrow(data)) {
    home <- data$homeTeam[i]
    away <- data$awayTeam[i]
    elo_h <- elo[home]
    elo_a <- elo[away]
    exp_h <- 1 / (1 + 10^((elo_a - elo_h - 100) / 400))
    if (has_scores && !is.na(data$homePoints[i]) && !is.na(data$awayPoints[i])) {
      result <- ifelse(data$homePoints[i] > data$awayPoints[i], 1, 0)
      elo[home] <- elo_h + k * (result - exp_h)
      elo[away] <- elo_a + k * ((1 - result) - (1 - exp_h))
    }
    data$elo_diff[i] <- elo[home] - elo[away]
  }
  return(data)
}

# Engineer features with API data
engineer_features <- function(game_data, team_stats, update_elo_flag = TRUE) {
  setDT(game_data)
  setDT(team_stats)
  
  # Preserve original game_data for scores
  original_game_data <- copy(game_data)
  
  has_scores <- "homePoints" %in% names(game_data) && "awayPoints" %in% names(game_data)
  
  if (update_elo_flag) {
    game_data <- update_elo(game_data)
  }
  
  # Merge scores back if scores are available
  if (has_scores) {
    game_data[original_game_data, on = .(season, startDate, homeTeam, awayTeam), 
              `:=`(homePoints = i.homePoints, awayPoints = i.awayPoints)]
  }
  
  # Merge team stats with game data
  game_data[team_stats, on = .(season = season, homeTeam = team), 
            `:=`(home_pace = i.pace, home_teamStats.trueShooting = i.teamStats.trueShooting,
                 home_teamStats.fourFactors.effectiveFieldGoalPct = i.teamStats.fourFactors.effectiveFieldGoalPct,
                 home_teamStats.fourFactors.turnoverRatio = i.teamStats.fourFactors.turnoverRatio,
                 home_teamStats.fourFactors.offensiveReboundPct = i.teamStats.fourFactors.offensiveReboundPct,
                 home_teamStats.fourFactors.freeThrowRate = i.teamStats.fourFactors.freeThrowRate,
                 home_opponentStats.trueShooting = i.opponentStats.trueShooting,
                 home_teamStats.points.total = i.teamStats.points.total,
                 home_teamStats.points.inPaint = i.teamStats.points.inPaint,
                 home_teamStats.points.offTurnovers = i.teamStats.points.offTurnovers,
                 home_teamStats.points.fastBreak = i.teamStats.points.fastBreak)]
  
  game_data[team_stats, on = .(season = season, awayTeam = team), 
            `:=`(away_pace = i.pace, away_teamStats.trueShooting = i.teamStats.trueShooting,
                 away_teamStats.fourFactors.effectiveFieldGoalPct = i.teamStats.fourFactors.effectiveFieldGoalPct,
                 away_teamStats.fourFactors.turnoverRatio = i.teamStats.fourFactors.turnoverRatio,
                 away_teamStats.fourFactors.offensiveReboundPct = i.teamStats.fourFactors.offensiveReboundPct,
                 away_teamStats.fourFactors.freeThrowRate = i.teamStats.fourFactors.freeThrowRate,
                 away_opponentStats.trueShooting = i.opponentStats.trueShooting,
                 away_teamStats.points.total = i.teamStats.points.total,
                 away_teamStats.points.inPaint = i.teamStats.points.inPaint,
                 away_teamStats.points.offTurnovers = i.teamStats.points.offTurnovers,
                 away_teamStats.points.fastBreak = i.teamStats.points.fastBreak)]
  
  # Calculate rest days
  team_game_dates <- rbind(game_data[, .(team = homeTeam, date = startDate)],
                           game_data[, .(team = awayTeam, date = startDate)])
  team_game_dates <- team_game_dates[order(team, date)]
  team_game_dates[, rest_days := as.numeric(difftime(date, shift(date), units = "days")), by = team]
  team_game_dates[, rest_days := replace(rest_days, is.na(rest_days), 0)]
  
  game_data[team_game_dates, on = .(homeTeam = team, startDate = date), home_rest_days := i.rest_days]
  game_data[team_game_dates, on = .(awayTeam = team, startDate = date), away_rest_days := i.rest_days]
  
  # Calculate momentum only if scores are available
  if (has_scores) {
    team_games <- melt(game_data[, .(startDate, homePoints, awayPoints, homeTeam, awayTeam)], 
                       id.vars = c("startDate", "homePoints", "awayPoints"), 
                       measure.vars = c("homeTeam", "awayTeam"), 
                       value.name = "team", 
                       variable.name = "team_type")
    team_games[, score_diff := fifelse(team_type == "homeTeam", homePoints - awayPoints, awayPoints - homePoints)]
    team_games <- team_games[, .(startDate, team, score_diff)]
    team_games <- team_games[order(team, startDate)]
    team_games[, momentum := frollmean(score_diff, n = 5, align = "right", fill = NA), by = team]
    
    game_data[team_games, on = .(homeTeam = team, startDate), home_momentum := i.momentum]
    game_data[team_games, on = .(awayTeam = team, startDate), away_momentum := i.momentum]
  } else {
    game_data[, `:=`(home_momentum = NA_real_, away_momentum = NA_real_)]
  }
  
  # Combine features
  game_data[, `:=`(
    eff_diff = replace(home_teamStats.points.total - away_teamStats.points.total, 
                       is.na(home_teamStats.points.total - away_teamStats.points.total), 0),
    pace = replace((home_pace + away_pace) / 2, 
                   is.na((home_pace + away_pace) / 2), 
                   replace(home_pace, is.na(home_pace), replace(away_pace, is.na(away_pace), 0))),
    momentum_diff = replace(home_momentum - away_momentum, is.na(home_momentum - away_momentum), 0),
    fatigue = pmin(replace(home_rest_days, is.na(home_rest_days), 0), 
                   replace(away_rest_days, is.na(away_rest_days), 0)),
    efg_diff = replace(home_teamStats.fourFactors.effectiveFieldGoalPct - away_teamStats.fourFactors.effectiveFieldGoalPct, 
                       is.na(home_teamStats.fourFactors.effectiveFieldGoalPct - away_teamStats.fourFactors.effectiveFieldGoalPct), 0),
    ts_diff = replace(home_teamStats.trueShooting - away_teamStats.trueShooting, 
                      is.na(home_teamStats.trueShooting - away_teamStats.trueShooting), 0),
    tov_diff = replace(home_teamStats.fourFactors.turnoverRatio - away_teamStats.fourFactors.turnoverRatio, 
                       is.na(home_teamStats.fourFactors.turnoverRatio - away_teamStats.fourFactors.turnoverRatio), 0),
    orb_diff = replace(home_teamStats.fourFactors.offensiveReboundPct - away_teamStats.fourFactors.offensiveReboundPct, 
                       is.na(home_teamStats.fourFactors.offensiveReboundPct - away_teamStats.fourFactors.offensiveReboundPct), 0),
    ftr_diff = replace(home_teamStats.fourFactors.freeThrowRate - away_teamStats.fourFactors.freeThrowRate, 
                       is.na(home_teamStats.fourFactors.freeThrowRate - away_teamStats.fourFactors.freeThrowRate), 0),
    paint_diff = replace(home_teamStats.points.inPaint - away_teamStats.points.inPaint, 
                         is.na(home_teamStats.points.inPaint - away_teamStats.points.inPaint), 0),
    fastbreak_diff = replace(home_teamStats.points.fastBreak - away_teamStats.points.fastBreak, 
                             is.na(home_teamStats.points.fastBreak - away_teamStats.points.fastBreak), 0),
    off_to_diff = replace(home_teamStats.points.offTurnovers - away_teamStats.points.offTurnovers, 
                          is.na(home_teamStats.points.offTurnovers - away_teamStats.points.offTurnovers), 0),
    bias_proxy = fifelse(homeTeam %in% c("Duke", "Kentucky", "UNC"), 0.1, -0.1)
  )]

  # Assign moneyline and spread conditionally
  if (has_scores) {
    game_data[, `:=`(
      moneyline = fifelse(homePoints > awayPoints, 1, 0),
      spread = homePoints - awayPoints
    )]
  } else {
    game_data[, `:=`(
      moneyline = NA_integer_,
      spread = NA_real_
    )]
  }

  # Handle NAs selectively for features used in modeling
  game_data <- game_data[!is.na(eff_diff) & !is.na(pace) & !is.na(momentum_diff) & !is.na(fatigue) & 
                         !is.na(efg_diff) & !is.na(ts_diff) & !is.na(tov_diff) & !is.na(orb_diff) & 
                         !is.na(ftr_diff) & !is.na(paint_diff) & !is.na(fastbreak_diff) & !is.na(off_to_diff)]
  
  return(game_data)
}

# Process past games with Elo updates
processed_data <- engineer_features(games_data, team_stats, update_elo_flag = TRUE)

# Calculate final Elo ratings from past games
final_elo <- calculate_final_elo(games_data)

# Prepare future games with precomputed Elo difference
setDT(future_games)
future_games[, elo_diff := final_elo[homeTeam] - final_elo[awayTeam]]
future_games[is.na(elo_diff), elo_diff := 0]  # Handle new teams not in past data

# Process future games without updating Elo (since no scores are available)
processed_data_current <- engineer_features(future_games, team_stats, update_elo_flag = FALSE)

# Save processed data to disk
saveRDS(processed_data, "processed_data.rds")
saveRDS(processed_data_current, "processed_data_current.rds")

# Define features
features <- c("elo_diff", "eff_diff", "pace", "momentum_diff", "fatigue", "efg_diff", "ts_diff", 
              "tov_diff", "orb_diff", "ftr_diff", "paint_diff", "fastbreak_diff", "off_to_diff", "bias_proxy")

# Train/test split for historical data
train_index <- createDataPartition(processed_data$moneyline, p = 0.8, list = FALSE)
train_data <- processed_data[train_index]
test_data <- processed_data[-train_index]

# Prepare CatBoost pools
cat_train <- catboost.load_pool(as.matrix(train_data[, ..features]), train_data$moneyline)
cat_test <- catboost.load_pool(as.matrix(test_data[, ..features]))

# Train CatBoost model (reduced complexity)
cat_model <- catboost.train(cat_train, params = list(
  iterations = 100,
  depth = 3,
  learning_rate = 0.05,
  loss_function = "Logloss",
  eval_metric = "AUC",
  verbose = 0
))

# Train Quantile Regression Forest model (reduced trees)
qrf_model <- quantregForest(x = as.data.frame(train_data[, ..features]), y = train_data$spread, ntree = 200)

# Save models to disk
catboost.save_model(cat_model, "cat_model.cbm")
saveRDS(qrf_model, "qrf_model.rds")

# Generate predictions for current season
cat_current <- catboost.load_pool(as.matrix(processed_data_current[, ..features]))
pred_data <- as.data.table(processed_data_current)[, 
  `:=`(
    money_prob = catboost.predict(cat_model, cat_current, prediction_type = "Probability"),
    spread_pred = predict(qrf_model, as.data.frame(processed_data_current[, ..features]), what = mean),
    spread_lower = predict(qrf_model, as.data.frame(processed_data_current[, ..features]), what = 0.1),
    spread_upper = predict(qrf_model, as.data.frame(processed_data_current[, ..features]), what = 0.9)
  )
]

source("build_hefml_hoops_site.R")
build_hefml_hoops_site(pred_data,
                       output_file = "hefml_hoops_11_17.html")