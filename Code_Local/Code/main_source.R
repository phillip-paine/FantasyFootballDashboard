## Creating datasets:
# Phillip Paine

# working code - change later
setwd("D:/Phillip/GitHub/FantasyFootballDashboard/Code_Local/Code")
source('required_packages.R')
source('required_functions.R')

##########################
## READING DATA:

TeamCode <- read.csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2022-23/teams.csv", header = TRUE)
# TeamCode index not in correct alphabetical order? Leeds and Leicester are wrong way round?

# Swap index: This (and the fixes to playersmatchdata and fixtures) are not robust to changes in the original code 
# ideally should use week 1 matches to ensure correct indexing but probably won't be changed now. Check this at start of code?
TeamCode[TeamCode$name == "Leeds", ]$id = 10
TeamCode[TeamCode$name == "Leicester", ]$id = 11

PlayersMatchData <- read.csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2022-23/gws/merged_gw.csv", header = TRUE)
# opponent_team index also incorrect:
PlayersMatchData$opponent_team <- ifelse(PlayersMatchData$opponent_team == 10, 11, 
                                         ifelse(PlayersMatchData$opponent_team == 11, 10, PlayersMatchData$opponent_team))
# Fix names:
PlayersMatchData$name <- iconv(PlayersMatchData$name, from="UTF-8", to="LATIN1")

Fixtures <- read.csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2022-23/fixtures.csv", header = TRUE)
# swap index (Leicester and Leeds are in wrong order)
Fixtures$team_a <- ifelse(Fixtures$team_a == 10, 11,
                           ifelse(Fixtures$team_a == 11, 10, Fixtures$team_a))
Fixtures$team_h <- ifelse(Fixtures$team_h == 10, 11,
                           ifelse(Fixtures$team_h == 11, 10, Fixtures$team_h))
########################################
## Parameters:
# write json to file 
temp = Fixtures %>% group_by(event) %>% summarise(max_fin = max(finished)) %>% filter(max_fin == "True")
latest_gw = max(temp$event)
list_params <- list(latest_gw = max(temp$event), next_gw = max(temp$event)+1, num_adapt=150, num_iter=1500, num_mTry=3,num_nodeSize=10,
                    num_nTree=150, promoted_attcoef=-0.4124, promoted_defcoef=0.2146)

json_params <- toJSON(list_params)
write(json_params, "set_parameters.json") # store as json to read in the app.R file

####################################
## Create Model Datasets:

# Check that Leeds and Leicester are still in wrong order and fix:
# move code from above (to do)

df_output <- main_source(Fixtures, TeamCode, PlayersMatchData, list_params)

# Compile Output:

df_results <- df_output$df_fixture
write.csv(df_results, paste("D:/Phillip/GitHub/FantasyFootballDashboard/Code_Local/Data/ResultsPredicted_", finished_gw, ".csv", sep = ""))
df_table <- df_output$df_finaltable
write.csv(df_table, paste("D:/Phillip/GitHub/FantasyFootballDashboard/Code_Local/Data/TablePredicted_", finished_gw, ".csv", sep = ""))
df_players <- df_output$predictionDataset
write.csv(df_players, paste("D:/Phillip/GitHub/FantasyFootballDashboard/Code_Local/Data/PlayersFFPredicted_", finished_gw, ".csv", sep = ""))
df_playersStats <- df_output$df_playerPoints
write.csv(df_playersStats, paste("D:/Phillip/GitHub/FantasyFootballDashboard/Code_Local/Data/PlayersFFStats_", finished_gw, ".csv", sep = ""))


