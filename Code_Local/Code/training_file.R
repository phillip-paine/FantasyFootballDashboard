## Training file:
# File to train the random forest model 

# Need to tidy this script:
#
# Create train-test splits:
# modelr isn't quite appropriate here but note for future reference (modelr part of tidyverse)

training_rounds <- seq(2, latest_gw - 2) # we don't model round 1 because na values
validation_rounds <- c(latest_gw - 1, latest_gw)

training_data <- feature_playerData %>% ungroup() %>% filter(round %in% training_rounds) %>% 
  select(total_points, was_home, total_points_lag1, total_points_lag2,
         total_points_lag3, oppo_attcoef, oppo_defcoef, team_attcoef, team_defcoef,
         rolling3avg_tp, rolling3_oppadjusted_atttp, rolling3_oppadjusted_deftp,
         rolling3_total_tp, transfers_balance_lag, value_lag, bonus_lag, bps_lag, position) %>% drop_na()

validation_data <- feature_playerData %>% ungroup() %>% filter(round %in% validation_rounds) %>% 
  select(total_points, was_home, total_points_lag1, total_points_lag2,
         total_points_lag3, oppo_attcoef, oppo_defcoef, team_attcoef, team_defcoef,
         rolling3avg_tp, rolling3_oppadjusted_atttp, rolling3_oppadjusted_deftp,
         rolling3_total_tp, transfers_balance_lag, value_lag, bonus_lag, bps_lag, position) %>% drop_na()

validation_feature <- validation_data %>% select(-total_points)

# Validation Model for Tuning Parameters:

modeltune <- randomForest::randomForest(total_points ~ was_home + total_points_lag1 + total_points_lag2 + total_points_lag3 + 
                                          oppo_attcoef + oppo_defcoef + team_attcoef + team_defcoef + rolling3avg_tp +
                                          rolling3_oppadjusted_atttp + rolling3_oppadjusted_deftp + rolling3_total_tp +
                                          transfers_balance_lag + value_lag + bonus_lag + bps_lag + position,
                                        xtest = validation_feature, ytest = validation_data$total_points,
                                        data = training_data, na.action = na.omit, importance = TRUE,
                                        ntree=150, replace=TRUE, nodesize=10, do.tree = 10, mTry = 3)

# tunerf : function to find mtry - mtry is the number of variables used in each tree
trf <- tuneRF(training_data %>% select(-total_points), training_data$total_points, plot = TRUE, trace = TRUE, improve = 0.05, ntreeTry = 75)
# mTry : 3

varImpPlot(modeltune, sort = T, n.var = 10, main = "Top 10 - Variable Importance")
importance(modeltune)
plot(modeltune)

# find optimum node size
node_list <- c(5, 10, 25, 50)
node_mse <- c(0,0,0,0)
i = 1
for(nl in node_list){
  modeltune <- randomForest::randomForest(total_points ~ was_home + total_points_lag1 + total_points_lag2 + total_points_lag3 + 
                                            oppo_attcoef + oppo_defcoef + team_attcoef + team_defcoef + rolling3avg_tp +
                                            rolling3_oppadjusted_atttp + rolling3_oppadjusted_deftp + rolling3_total_tp +
                                            transfers_balance_lag + value_lag + bonus_lag + bps_lag + position,
                                          xtest = validation_feature, ytest = validation_data$total_points,
                                          data = training_data, na.action = na.omit, importance = TRUE,
                                          ntree=100, replace=TRUE, nodesize=nl, do.tree = 10, mTry = 3)
  node_mse[i] = min(modeltune$mse)
  i = i + 1
}
node_mse
# optimal node size: 10
# nodesize impacts num nodes too