## This script will calculate the team coefficents for the season ahead, including for newly promoted teams. 
# Likely use some reversion to the mean for the start of the season, i.e. previous season estimates and then brought closer to the avg.
# Promoted teams likely using estimate of promoted teams in the past. One thought is to use goals/concede from championship as a feature
# to estimate start of season values.

# This only needs to be run once at the start of the season to get the pre_season team strengths:

# promoted_team_strength <- function(){
#   
#   
#   
# }
# 
# start_of_season_strength <- function(){
#   
#   
#   
#   
# }

## Create data from past several seasons:
df_team_coef <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(df_team_coef) <- c("id", "name", "attcoef", "defcoef", "season")
seasons <- c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22")
# 16/17 and 17/18 don't have fixtures instead need to use merged_gw data - these plus 18/19 dont have teams dataset - need to create as well
seasons <- c("2018-19", "2019-20", "2020-21", "2021-22")
mcmc_n_adapt <- 250
mcmc_n_iters <- 5000

for(s in seasons){
  Fixtures <- read.csv(paste("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/", s, "/fixtures.csv", sep=""), header = TRUE)
  if(s %in% c("2018-19", "2017-18", "2016-17")){
    TeamCode <- read.csv(paste("D:/Phillip/GitHub/FantasyFootballDashboard/Code_Local/Data/TeamCode_", s, ".csv", sep=""), header=TRUE)
  }
  else{
    TeamCode <- read.csv(paste("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/", s, "/teams.csv", sep=""), header = TRUE)
    TeamCode <- TeamCode %>% select(c(id, name))
  }
  
 
  if(s == "2021-22"){ # need to correct id in this season
    # swap index 9 and 10 (Leicester and Leeds are in wrong order)
    Fixtures$team_a <- ifelse(Fixtures$team_a == 9, 10,
                              ifelse(Fixtures$team_a == 10, 9, Fixtures$team_a))
    Fixtures$team_h <- ifelse(Fixtures$team_h == 9, 10,
                              ifelse(Fixtures$team_h == 10, 9, Fixtures$team_h))
    TeamCode[TeamCode$name == "Leeds", ]$id = 9
    TeamCode[TeamCode$name == "Leicester", ]$id = 10
  }
  
  # Fit the results model to get the att/def coefficient for each season
  strength_output <- model_TeamStrength(Fixtures, 38, mcmc_n_adapt, mcmc_n_iters)
  df_strength = strength_output$df # return the dataframe from the model
  TeamCode <- merge(TeamCode, df_strength[, c("team_index", "attcoef", "defcoef")], by.x = c("id"), by.y = c("team_index")) # add to teamcode data
  TeamCode$season <- rep(s, 20) # add season identifier
  
  # save to dataframe:
  
  df_team_coef <- rbind(df_team_coef, TeamCode)
  
}

# 1. Get season att+def coefficients for the season after promotion for each team
# 2. Predict these values using the promotion season features

promoted_teams <- read.csv('D:/Phillip/GitHub/FantasyFootballDashboard/Code_Local/Data/PromotedTeams.csv')

df_coef <- merge(promoted_teams, df_team_coef, by.x=c("Team", "PremiershipSeason"), by.y=c("name", "season"), all.x=TRUE) # keep all entries

# Model att/def coef using features of promotion:
# preprocess data first?

lm_data <- df_coef[complete.cases(df_coef), ]
fit <- lm(attcoef ~ GF + GA + Pts + Pos + Bounced_Back, data = lm_data)
plot(fit)

fit <- lm(attcoef ~ Bounced_Back, data = lm_data)
summary(fit) # seems like only bounced_back is a useful predictor

fit <- lm(attcoef ~ GF, data = lm_data)
summary(fit) # 

# simpler model? avg. of previous season attcoef/defcoef? is this good enough?






