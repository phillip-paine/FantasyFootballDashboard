## Creating datasets:
# 08/11/2021
# Phillip Paine

# working code - change later
setwd("D:/Phillip/GitHub/FantasyFootballDashboard/Code_Local/Code")
source('required_packages.R')

##########################
## READING DATA:

# Pass url to read.csv function:
RawPlayers <- read.csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2022-23/players_raw.csv", header = TRUE)
# Create full name column as a unique identifier for players - need to adjust for duplicate names:
RawPlayers <- RawPlayers %>% filter(id == 248) # Ben Davies but on loan at Sheffield Utd so can fudge this for now
RawPlayers <- RawPlayers %>% mutate(full_name = paste(first_name, second_name, sep = "_"))
# write.csv(RawPlayers, "D:/Phillip/GitHub/FantasyFootball2122/FF2021_Analysis/Data/RawPlayersDataset_endofseason.csv")

CleanedPlayers <- read.csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2022-23/cleaned_players.csv", header = TRUE)
CleanedPlayers <- CleanedPlayers %>% mutate(full_name = paste(first_name, second_name, sep = "_"))
CleanedPlayers <- CleanedPlayers %>% filter(!(full_name == "Ben_Davies" & minutes == 0))

IdPlayers <- read.csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2022-23/player_idlist.csv", header = TRUE)
CleanedPlayers <- merge(CleanedPlayers, IdPlayers, by = c("first_name", "second_name"), all.x = TRUE)

TeamCode <- read.csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2022-23/teams.csv", header = TRUE)

# append to filenames to get team data:
teams <- c("Arsenal", "Aston_Villa", "Brentford", "Brighton", "Burnley", "Chelsea", "Crystal_Palace", "Everton", "Leeds",  "Leicester", "Liverpool", 
           "Manchester_City", "Manchester_United", "Newcastle_United", "Norwich", "Southampton", "Tottenham", "Watford",
           "West_Ham", "Wolverhampton_Wanderers")

for(t in teams){
  # replace space with underscore:
  t = sub(" ", "_", t)
  assign(paste("stats", t, sep=""),read.csv(paste("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2022-23/understat/understat_", t, ".csv", sep = ""),
                                            header = TRUE)) 
}

PlayersMatchData <- read.csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2022-23/gws/merged_gw.csv", header = TRUE)

Fixtures <- read.csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2022-23/fixtures.csv", header = TRUE)






