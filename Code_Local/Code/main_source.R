## Creating datasets:
# 08/11/2021
# Phillip Paine

# working code - change later
setwd("D:/Phillip/GitHub/FantasyFootballDashboard/Code_Local/Code")
source('required_packages.R')
source('required_functions.R')
source('required_parameters.R')

## To do:
# Change gw to kickoff time to model fixtures and predict future results + fantasy football player points
# better model for goals scored - zero-inflated or hurdle poisson model?
# scale or normalise FF model variables, e.g. team strength coefficients etc.

##########################
## READING DATA:

TeamCode <- read.csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2021-22/teams.csv", header = TRUE)
# TeamCode index not in correct alphabetical order? Leeds and Leicester are wrong way round?

# Swap index 9 amd 10: This (and the fixes to playersmatchdata and fixtures) are not robust to changes in the original code 
# ideally should use week 1 matches to ensure correct indexing but probably won't be changed now. Check this at start of code?
TeamCode[TeamCode$name == "Leeds", ]$id = 9
TeamCode[TeamCode$name == "Leicester", ]$id = 10

PlayersMatchData <- read.csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2021-22/gws/merged_gw.csv", header = TRUE)
# opponent_team index also incorrect:
PlayersMatchData$opponent_team <- ifelse(PlayersMatchData$opponent_team == 9, 10, 
                                         ifelse(PlayersMatchData$opponent_team == 10, 9, PlayersMatchData$opponent_team))
# Fix names:
PlayersMatchData$name <- iconv(PlayersMatchData$name, from="UTF-8", to="LATIN1")

Fixtures <- read.csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2021-22/fixtures.csv", header = TRUE)
# swap index 9 and 10 (Leicester and Leeds are in wrong order)
Fixtures$team_a <- ifelse(Fixtures$team_a == 9, 10,
                           ifelse(Fixtures$team_a == 10, 9, Fixtures$team_a))
Fixtures$team_h <- ifelse(Fixtures$team_h == 9, 10,
                           ifelse(Fixtures$team_h == 10, 9, Fixtures$team_h))

####################################
## Create Model Datasets:

# Check that Leeds and Leicester are still in wrong order and fix:
# move code from above (to do)

df_output <- main_source(Fixtures, TeamCode, PlayersMatchData, finished_gw, num_adapt, num_iter, num_mTry, num_nodeSize, num_nTree)

# Compile Output:

df_results <- df_output$df_fixture
write.csv(df_results, paste("D:/Phillip/GitHub/FantasyFootballDashboard/Code_Local/Data/ResultsPredicted_", finished_gw, ".csv", sep = ""))
df_table <- df_output$df_finaltable
write.csv(df_table, paste("D:/Phillip/GitHub/FantasyFootballDashboard/Code_Local/Data/TablePredicted_", finished_gw, ".csv", sep = ""))
df_players <- df_output$predictionDataset
write.csv(df_players, paste("D:/Phillip/GitHub/FantasyFootballDashboard/Code_Local/Data/PlayersFFPredicted_", finished_gw, ".csv", sep = ""))
df_playersStats <- df_output$df_playerPoints
write.csv(df_playersStats, paste("D:/Phillip/GitHub/FantasyFootballDashboard/Code_Local/Data/PlayersFFStats_", finished_gw, ".csv", sep = ""))


