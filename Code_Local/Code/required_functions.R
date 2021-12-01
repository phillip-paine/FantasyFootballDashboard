## Functions:

#####################################################
## Team Strength Coefficients:
# Model : could include champions league midweek, key player indicator, weather (rain/no rain for example)

model_TeamStrength <- function(data_fixtures, gameweek, n_ada, n_it){
  
  gw_seq <- seq(1, gameweek, by = 1)
  ngames <- nrow(data_fixtures[data_fixtures$event %in% gw_seq, ])
  nteam <- 20
  home_goals <- data_fixtures[data_fixtures$event %in% gw_seq, ]$team_h_score
  away_goals <- data_fixtures[data_fixtures$event %in% gw_seq, ]$team_a_score
  home_team <- data_fixtures[data_fixtures$event %in% gw_seq, ]$team_h #home team index 1-20
  away_team <- data_fixtures[data_fixtures$event %in% gw_seq, ]$team_a #away team index 1-20
  
  forJags <- list(y1 = home_goals, y2 = away_goals, hometeam = home_team, awayteam = away_team,
                  nteams = nteam, ngames = ngames)
  
  jagsmodel <- jags.model(file = "poissonModelTeamStrength.bug",  # compile model
                          data = forJags, n.adapt = n_ada, n.chains = 1)
  
  teamStr <- coda.samples(jagsmodel, variable.names=c("att", "def", "home_p"),  #parameter from truncated poisson
                          n.iter= n_it)
  
  mat.teamStr <- as.matrix(teamStr)
  homeadv <- mean(mat.teamStr[, 41]) # home advantage parameter 
  #homeadv = 0
  attcoef <- colMeans(mat.teamStr[, 1:20])
  defcoef <- colMeans(mat.teamStr[, 21:40])
  team_index <- seq(1, 20, by = 1)
  dt.coef <- data.frame(team_index, attcoef, defcoef)
  
  return(list("df" = dt.coef, "H" = homeadv)) 
}

###################################################################
## Add coefficents to fixture list:

fixtures_strength <- function(df_fixtures, strength_model){
  df_strength = strength_model$df # index names need fixing in function 
  home_adv = strength_model$H
  
  # Add coefficients to Fixture dataframe by team index:
  df_fixtures <- merge(df_fixtures, df_strength[, c("team_index", "attcoef", "defcoef")], by.x = c("team_h"), by.y = c("team_index"))
  df_fixtures <- df_fixtures %>% rename(home_attcoef = attcoef, home_defcoef = defcoef)
  df_fixtures <- merge(df_fixtures, df_strength[, c("team_index", "attcoef", "defcoef")], by.x = c("team_a"), by.y = c("team_index"))
  df_fixtures <- df_fixtures %>% rename(away_attcoef = attcoef, away_defcoef = defcoef)
  df_fixtures$HomeAdvantage <- rep(home_adv, nrow(df_fixtures))
  return(df_fixtures)
}


## Create final league table : a column for each gameweek starting from current table
# 

create_table <- function(df_fixtures, df_teamid, current_gw){
  df_leaguetable <- df_teamid[, c("name", "id")]
  temp_table <- update_table(df_fixtures, current_gw)
  df_leaguetable <- merge(df_leaguetable, temp_table, by = c("id"))
  remove(temp_table)
  for(k in (current_gw+1):38){
    temp_table <- update_table(df_fixtures, k)
    df_leaguetable <- merge(df_leaguetable, temp_table, by = c("id"))
    remove(temp_table)
  }
  df_leaguetable <- df_leaguetable %>% select(-id) %>% arrange(desc(Points_38)) # rearrange by final week in season
  return(df_leaguetable)
}

# Create column of points for a given gameweek:
update_table <- function(df_fixtures, gw){ # create table up to gw:
  Home_Results <- df_fixtures %>% filter(event <= gw) %>% group_by(team_h) %>% summarise(H_points = sum(homepred_points)) 
  Away_Results <- df_fixtures %>% filter(event <= gw) %>% group_by(team_a) %>% summarise(A_points = sum(awaypred_points))
  Results <- merge(Home_Results, Away_Results, by.x = c("team_h"), by.y = c("team_a"))
  Results <- Results %>% rename(id = team_h)
  Results[[paste("Points", gw, sep = "_")]] <- Results$H_points + Results$A_points
  Results <- Results %>% select(id, starts_with("Points_"))
  # df_combined <- Results %>% select(name, Points) %>% arrange(desc(Points)) # don't need to arrange here
  return(Results) # output team index 1...20 with predicted points for each gameweek (or actual points for elapsed game weeks)
}

# predict match score from poisson model with team strength as parameters + home_advantage
goals_scored <- function(x){
  goals = round(x, digits = 0)
  return(goals)
}

MC_result <- function(h, a){
  MC_n = 200
  result_table <- data.frame(matrix("NA", ncol = 2, nrow = MC_n)) # hard code in the monte carlo simulations = 200 (this is enough)
  result_table$X1 <- rpois(MC_n, h)
  result_table$X2 <- rpois(MC_n, a)
  result_table$outcome <- if_else(result_table$X1 > result_table$X2, "H", 
                                  if_else(result_table$X1 == result_table$X2, "D", "A"))
  result_table <- result_table %>% summarise(home_win = sum(outcome == "H")/MC_n, away_win = sum(outcome == "A")/MC_n, 
                                             draw = sum(outcome == "D")/MC_n)
  
  result_table <- result_table %>%  mutate(result = case_when((home_win > draw) & (home_win > away_win) ~ "H", 
                                                              (home_win < draw) & (draw > away_win) ~ "D",
                                                              (away_win > draw) & (away_win > home_win) ~ "A"))
  result_table <- result_table %>% mutate(result = replace_na(result, "D")) # if ties then show draw as result
  return(result_table)
}

## Predicted Score: Not needed for the modelling but use in dashboard page
# For each future match calculate the expected goals -> predicted points -> create league table
# Later update : Monte Carlo simulation of final league points and positions with variance, best/worst finish etc.
predict_MatchScore <- function(df_fixture, current_gw){
  
  df_fixture <- df_fixture %>% mutate(home_lambda = exp(HomeAdvantage + home_attcoef + away_defcoef), away_lambda = exp(away_attcoef + home_defcoef))
  # If we wanted to use Monte Carlo we can change this function to use the poisson distribution
  
  # New Code:
  # 200 draws per game is plenty:
  # only need to do case where event > current_gw and then use team_a/h_score for <= current_gw
  # output home_win%, draw% and away_win% for each unfinished fixture
  result_outcome <- matrix(NA, nrow = nrow(df_fixture), ncol = 4)
  result_outcome <- mapply(MC_result, df_fixture$home_lambda, df_fixture$away_lambda)
  df_outcomes <- data.frame(matrix(unlist(result_outcome), nrow = nrow(df_fixture),ncol = 4, byrow = TRUE))
  colnames(df_outcomes) <- c("Home_win", "Away_win", "Draw", "PredictOutcome")
  df_fixture <- bind_cols(df_fixture, df_outcomes)
  
  ## Old Code: Use expected goals to decide fixtures - too many draws as most lambda round to 1.
  #df_fixture$homegoals_pred <- sapply(df_fixture$home_lambda, function(x) goals_scored(x))
  #df_fixture$awaygoals_pred <- sapply(df_fixture$away_lambda, function(x) goals_scored(x))
  #df_fixture$predict_gw <- rep(current_gw, nrow(df_fixture))
  
  #df_fixture <- df_fixture %>% mutate(homegoals = case_when(event <= predict_gw ~ team_h_score, event > predict_gw ~ homegoals_pred),
  #                                awaygoals = case_when(event <= predict_gw ~ team_a_score, event > predict_gw ~ awaygoals_pred))
  
  df_fixture$ActualOutcome <- if_else(df_fixture$team_h_score > df_fixture$team_a_score, "H",
                                      if_else(df_fixture$team_h_score == df_fixture$team_a_score, "D", "A"))
  df_fixture$Outcome <- if_else(df_fixture$finished == "True", df_fixture$ActualOutcome, df_fixture$PredictOutcome)
  
  df_fixture <- df_fixture %>% select(-PredictOutcome, ActualOutcome)
  
  # Predict a one-off match for the league table:
  df_fixture$predict_team_h_score <- sapply(df_fixture$home_lambda, function(x) rpois(1, x))
  df_fixture$predict_team_a_score <- sapply(df_fixture$away_lambda, function(x) rpois(1, x))
  
  df_fixture$predict_team_h_score <- sapply(df_fixture$predict_team_h_score, as.numeric)
  df_fixture$predict_team_a_score <- sapply(df_fixture$predict_team_a_score, as.numeric)
  
  df_fixture$predict_team_h_score <- if_else(df_fixture$event <= current_gw, df_fixture$team_h_score, df_fixture$predict_team_h_score)
  df_fixture$predict_team_a_score <- if_else(df_fixture$event <= current_gw, df_fixture$team_a_score, df_fixture$predict_team_a_score)
  
  return(df_fixture)
}

# Calculate match result from expected goals in predict_MatchScore function output
#matchResult <- function(){
#  
#  return(df_result) # fixture list with completed matches and predicted match points - calculate from expected goals for each team
#}

################################################################
## Fantasy Points Modelling: Features
# model to calculate the predicted fantasy point earned in a gameweek for each player
# Features to include in the model - lagged points, transfer in/out, form (recent points), recent points against opp att/def coefficients,
# played champions league midweek, ... 
# 3 or 5 week rolling window for points, transfer in/out etc?

# need to deal with injury news

create_features <- function(pm_data, df_next, team_str, latest_gw){
  
  pm_data <- pm_data %>% select(name, position, team, bonus, bps, assists, goals_scored, ict_index, team_id, opponent_id, GW, total_points, selected,
                                transfers_balance, transfers_in, transfers_out, value, was_home, round, red_cards, yellow_cards)
  # Create next fixture rows:
  pm_next <- pm_data %>% filter(round == latest_gw)
  pm_next$round <- rep(latest_gw + 1, nrow(pm_next)) # change to first future round
  pm_next$GW <- rep(latest_gw + 1, nrow(pm_next))
  pm_next <- merge(pm_next, df_next %>% select(home_team, team_a), by.x = c("team"), by.y = c("home_team"), all.x = TRUE) # add fixture with opponent_tream id to merge
  pm_next <- merge(pm_next, df_next %>% select(away_team, team_h), by.x = c("team"), by.y = c("away_team"), all.x = TRUE)
  pm_next$opponent_id <- if_else(is.na(pm_next$team_h), pm_next$team_a, pm_next$team_h)
  pm_next$was_home <- if_else(is.na(pm_next$team_h), "True", "False")
  pm_next <- pm_next %>% select(name, position, team, bonus, bps, assists, goals_scored, ict_index, team_id, opponent_id, GW, total_points, selected,
                                transfers_balance, transfers_in, transfers_out, value, was_home, round)
  
  # cbind next fixture data:
  pm_data <- bind_rows(pm_data, pm_next)
  
  # merge att+def coefficients by team: merge is not corret with home = team?
  pm_data <- merge(pm_data, team_str, by.x = c("team_id"), by.y = c("team_index"))
  pm_data <- pm_data %>% rename(team_attcoef = attcoef, team_defcoef = defcoef)
  pm_data <- merge(pm_data, team_str, by.x =c("opponent_id"), by.y = c("team_index"))
  pm_data <- pm_data %>% rename(oppo_attcoef = attcoef, oppo_defcoef = defcoef)
  
  pm_data$was_home <- ifelse(pm_data$was_home == "True", 1, 0) # create numeric flag
  
  # Predicting each week total_points earned by player : create lead values so that we predict from last week's points etc.
  # week 2 - 4 should be using all of previous week data and then from week 5+ we use five week rolling windows
  
  pm_data <- pm_data %>% group_by(name) %>% mutate(total_points_lag1 = lag(total_points, order_by = round), 
                                                   total_points_lag2 = lag(total_points, n = 2, order_by = round),
                                                   total_points_lag3 = lag(total_points, n = 3, order_by = round))
  pm_data <- pm_data %>% group_by(name) %>% mutate(opp_attcoef_lag1 = lag(oppo_attcoef, order_by = round), 
                                                   opp_attcoef_lag2 = lag(oppo_attcoef, n = 2, order_by = round),
                                                   opp_attcoef_lag3 = lag(oppo_attcoef, n = 3, order_by = round),
                                                   opp_defcoef_lag1 = lag(oppo_defcoef, order_by = round), 
                                                   opp_defcoef_lag2 = lag(oppo_defcoef, n = 2, order_by = round),
                                                   opp_defcoef_lag3 = lag(oppo_defcoef, n = 3, order_by = round))
  pm_data <- pm_data %>% rowwise() %>% mutate(rolling3avg_tp = mean(c(total_points_lag1, total_points_lag2, total_points_lag3), na.rm = TRUE),
                                              rolling3avg_oppatt = mean(c(opp_attcoef_lag1, opp_attcoef_lag2, opp_attcoef_lag3), na.rm = TRUE),
                                              rolling3avg_oppdef = mean(c(opp_defcoef_lag1, opp_defcoef_lag2, opp_defcoef_lag3), na.rm = TRUE))
  pm_data$rolling3_oppadjusted_atttp <- pm_data$rolling3avg_tp * pm_data$rolling3avg_oppatt 
  pm_data$rolling3_oppadjusted_deftp <- pm_data$rolling3avg_tp * pm_data$rolling3avg_oppdef 
  pm_data$rolling3_total_tp <- pm_data$total_points_lag1 + pm_data$total_points_lag2 + pm_data$total_points_lag3
  pm_data <- pm_data %>% group_by(name) %>% mutate(transfers_balance_lag = lag(transfers_balance, order_by = round),
                                                   selected_lag = lag(selected, order_by = round), value_lag = lag(value, order_by = round),
                                                   bonus_lag = lag(bonus, order_by = round), bps_lag = lag(bps, order_by = round))
  
  pm_data <- pm_data %>% group_by(name) %>% mutate(ictindex_lag1 = lag(ict_index, order_by = round), 
                                                   ictindex_lag2 = lag(ict_index, n = 2, order_by = round),
                                                   ictindex_lag3 = lag(ict_index, n = 3, order_by = round))
  pm_data <- pm_data %>% mutate(rolling3_avg_ictindex = mean(c(ictindex_lag1, ictindex_lag2, ictindex_lag3), na.rm = TRUE))
  
  pm_data <- pm_data %>% mutate(red_card_lag = lag(red_cards, order_by = round)) #red card = miss next game
  
  # can add news of missing next game : suspension, injury etc.
  
  # function of points and att/def coef in recent matches? def for gk+def and att for mid+fwd?
  
  pm_data$weighted_tp_att_previous <- pm_data$total_points_lag1 * pm_data$opp_attcoef_lag1
  pm_data$weighted_tp_def_previous <- pm_data$total_points_lag1 * pm_data$opp_defcoef_lag1
 
  return(pm_data)
}


####################################################
## Main Source File:

main_source <- function(df_fixture, df_teams, df_playersMatch, latest_gw, num_adapt_mcmc, num_iters_mcmc, rf_nTree, rf_nodeSize, rf_mTry){
  #####################################
  ## Team Modelling:
  
  ## Team Strength Model:
  
  strength_output <- model_TeamStrength(df_fixture, latest_gw, num_adapt_mcmc, num_iters_mcmc)
  
  df_fixture <- fixtures_strength(df_fixture, strength_output)
  
  ## Predict Score: Include home advantage
  
  df_fixture <- predict_MatchScore(df_fixture, latest_gw)
  
  ## Points:
  
  df_fixture <- df_fixture %>% mutate(homepred_points = case_when(predict_team_h_score > predict_team_a_score ~ 3, predict_team_h_score == predict_team_a_score ~ 1, predict_team_h_score < predict_team_a_score ~ 0),
                                  awaypred_points = case_when(predict_team_h_score < predict_team_a_score ~ 3, predict_team_h_score == predict_team_a_score ~ 1, predict_team_h_score > predict_team_a_score ~ 0))
  
  ## Create League Table:
  
  df_finaltable <- create_table(df_fixture, df_teams, latest_gw)
  # create a gameweek slider on dashboard
  
  # rename fixture columns:
  df_fixture <- merge(df_fixture, TeamCode[, c("id", "name")], by.x = c("team_h"), by.y = c("id"))
  df_fixture <- df_fixture %>% rename(home_team = name)
  df_fixture <- merge(df_fixture, TeamCode[, c("id", "name")], by.x = c("team_a"), by.y = c("id"))
  df_fixture <- df_fixture %>% rename(away_team = name)
  
  #####################################
  ## Fantasy Football Points Model:
  
  df_playersMatch <- merge(df_playersMatch, df_teams[, c("name", "id")], by.x = "team", by.y = "name")
  df_playersMatch <- df_playersMatch %>% rename(team_id = id, opponent_id = opponent_team)
  
  # Create Features:
  next_fixtures <- df_fixture %>% filter(event == (latest_gw + 1)) # next set of fixtures by event/gw
  feature_playerData <- create_features(df_playersMatch, next_fixtures, strength_output$df, latest_gw)
  # need to add binning of features to improve performance? Also probably standardise features to make separation of groups more distinct
  
  
  # Final Model Run : All Training Data:
  model_data <- feature_playerData %>% ungroup() %>% filter(round <= latest_gw) %>% 
    select(total_points, was_home, total_points_lag1, total_points_lag2,
           total_points_lag3, oppo_attcoef, oppo_defcoef, team_attcoef, team_defcoef,
           rolling3avg_tp, rolling3_oppadjusted_atttp, rolling3_oppadjusted_deftp,
           rolling3_total_tp, transfers_balance_lag, value_lag, bonus_lag, bps_lag, position)
  
  
  modelRF <- randomForest::randomForest(total_points ~ was_home + total_points_lag1 + total_points_lag2 + total_points_lag3 + 
                                          oppo_attcoef + oppo_defcoef + team_attcoef + team_defcoef + rolling3avg_tp +
                                          rolling3_oppadjusted_atttp + rolling3_oppadjusted_deftp + rolling3_total_tp +
                                          transfers_balance_lag + value_lag + bonus_lag + bps_lag + position, 
                                        data = model_data, na.action = na.omit, ntree=rf_nTree, replace=TRUE, nodesize=rf_nodeSize, mTry=rf_mTry)
  # plot(modelRF)
  
  # Fixture dataset to return:
  df_fixture <- df_fixture %>% select(event, home_team, Home_win, Draw, Away_win, away_team)
  
  # Fantasy Predictions for Future Gameweek:
  # FOr now we just predict one future week : could do avg. three games etc
  predictionDataset <- feature_playerData %>% ungroup() %>% filter(round == (latest_gw + 1)) %>% 
    select(name, team, total_points, was_home, total_points_lag1, total_points_lag2, total_points_lag3, oppo_attcoef, oppo_defcoef, team_attcoef, team_defcoef,
           rolling3avg_tp, rolling3_oppadjusted_atttp, rolling3_oppadjusted_deftp, rolling3_total_tp, transfers_balance_lag, value_lag, 
           bonus_lag, bps_lag, position, round)
  
  pred_output <- predict(modelRF, predictionDataset)
  
  predictionDataset$predicted_total_points <- pred_output
  
  predictionDataset <- predictionDataset %>% select(name, team, position, predicted_total_points, round)
  predictionDataset <- merge(predictionDataset, df_playersMatch %>% filter(round == latest_gw) %>% select(name, value), by = c("name"))
  predictionDataset$current_value <- predictionDataset$value / 10
 
  df_playerPoints <- df_playersMatch %>% filter(round <= latest_gw) %>% select(name, position, total_points) %>%
    group_by(name, position) %>% summarise(total_points = sum(total_points))
  df_playerPoints <- merge(df_playerPoints, df_playersMatch %>% filter(round == latest_gw) %>% select(name, value), by = c("name"))
  df_playerPoints$current_value <- df_playerPoints$value/10 # convert to millions
  df_playerPoints <- df_playerPoints %>% select(-value)
  
  df_playerForm <- df_playersMatch %>% filter(round %in% seq(latest_gw - 4, latest_gw)) %>% select(name, position, total_points) %>%
    group_by(name, position) %>% summarise(form_total_points = sum(total_points))
  df_playerPoints <- merge(df_playerPoints, df_playerForm, by = c("name", "position"), all.x = TRUE)
  
  return(list(df_fixture = df_fixture, df_finaltable = df_finaltable, predictionDataset = predictionDataset, df_playerPoints = df_playerPoints))
}



