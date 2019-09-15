##################################################################################
##################################################################################
##
##  Jose Jimenez
##  Ice Hockey Data Analysis
##
##################################################################################
##################################################################################

library(tidyverse)
library(ggplot2)
library(lattice)
R.Version()$version.string # print R version
# Always remember to set working directory to the file containing folder
# setwd("~path")!
setwd("######Betting_models")



##################################################################################
##################################################################################
##
##  Exploratory Data Analysis & Preparation
##
##################################################################################
##################################################################################

# Loading data from csv provided by Tipico Quant team.
df <- read.csv("Icehockey_OU_data_3000.csv", header=TRUE, sep=",")

# Apply build-in functions to explore the characteristics of the provided data.
#head(df)
#tail(df)
#str(df)
#summary(df)
#is.na(df)


# Looking for outliers/wrong data

# Box plot
par(mfrow=c(1,1))
boxplot(df[,22:24],ylab="Odds",main="Regular time matchodds", 
        col=topo.colors(3))

# Histogram Tipp1
hist(df$Tipp1, 
     main="Odds", 
     xlab="Tip1", 
     border="blue", 
     col="green",
     freq = TRUE,
     xlim=c(1,50),
     ylim=c(1,100),
     las=0,breaks=50)

# Histogram TippX
hist(df$TippX, 
     main="Odds", 
     xlab="TippX", 
     border="blue", 
     col="red",
     freq = TRUE,
     xlim=c(1,25),
     ylim=c(1,100),
     las=0,breaks=50)

# Histogram Tipp2
hist(df$Tipp2, 
     main="Odds", 
     xlab="Tipp2", 
     border="blue", 
     col="orange",
     freq = TRUE,
     xlim=c(1,50),
     ylim=c(1,100),
     las=0,breaks=50)


# Data issues : We can find lots of NAs, special characters in 
# team names when reading in, ...

# 1. Create a list of the involved teams.
# a. Extract and merge all team names from columns Team1 & 2
teamList <- unlist(lapply(df[, c("Team1", "Team2")], as.character), 
                   use.names = FALSE)

# b. Remove Duplicates
teamList <- unique(teamList)

# 2. Create a vector where we specify the suggested range of variables 
#    used to compute our odds.

teamStats <- c('local_goals', 'foreign_goals', 'local_conceded', 
             'foreign_conceded', 'local_games', 
             'foreign_games', 'avg_local_goals', 'avg_foreign_goals', 
             'Attack_Ratio_local', 'Defend_Ratio_local', 
             'Attack_Ratio_foreign', 'Defend_Ratio_foreign')

# 3. Create the teamPerformance dataframe where the evolving Stats of 
# the teams will be stored.
n <- length(teamList)
m <- length(teamStats)
tmp_mat <- replicate(m, numeric(n))
teamPerformance <- data.frame(tmp_mat)
colnames(teamPerformance) <- c(teamStats)
rownames(teamPerformance) <- c(teamList)


# 4. Remove temporary variables
rm(n, m, tmp_mat)

##################################################################################
##################################################################################
##
##  Strategy
##
##################################################################################
##################################################################################

# We have a dataframe for all the games played and an empty dataframe for team 
# statistics.

# There are two ways to approach the odds computation. The dynamic case involves 
# creating the odds during the game based goals scores in each third of the game 
# OR the historical approach, in which odds are calculated based on previous games 
# played (or an hybrid, but outscope for this assignment). 

# The historical approach is chosen for simplicity.Howe,ver it is extremely 
# important to remark that this approach has the disadvantage of not being able 
# to compute odds before some games have actually been played (remember that
# WE NEED HISTORICAL DATA!).

# Therefore, the strategy here is to loop through each game over time and, 
# based on the information gathered up until that point, compute the different 
# O/U odds for the teams playing.

# The process works as follows:
#   1.) (By looping through rows) Read in game statistics of the game in df.
#   2.) Use stats in teamPerformance to compute the appropriate odds.
#   3.) Store the odds in the appropriate columns in df for each game.
#   4.) Use the statistics to update appropriate variables in teamPerformance.

# The requirement of the task given (1.b) was to explain the methodology for 
# the computation of (fair) O/U odds and create a function wrapper to implement it. 
# However, in this approach we attempt to crete a full analytics pipeline.

# We assume the fair odds are computed as (1/probability) to maintain expected 
# value of games equal 0 (fair). The are many ways to compute the probabilities 
# involved, i.e. using Bayesian methods to compute priori and posterior 
# distributions. However due to the scope and time limitations of the assignment, 
# simple Poisson method is used. Actually, in many team sports the Poisson 
# distribution alone is a relatively accurate predictor of Betting chances.

##################################################################################
##################################################################################
##
##  Function wrappers: Outcome probabilities & Odds
##
##################################################################################
##################################################################################

# Team_ratios_calculation()
# Inputs: Historical team performance results & stats
# Outputs: 4 ratios to approach the current potential performance of the team in 
# Attacking/Defending as a Local/Foreigner
 

Team_ratios_calculation <- function(local_goals, local_games, avg_local_goals,
                                    local_goals_conceded, 
                                    avg_foreign_goals, foreign_goals, 
                                    foreign_games, foreign_goals_conceded) {
  
  
  
  Attack_Ratio_local <- (local_goals/ local_games) / avg_local_goals
  Defend_Ratio_local <- (local_goals_conceded / local_games) / avg_foreign_goals
  
  
  Attack_Ratio_foreign <- (foreign_goals / foreign_games) / avg_foreign_goals
  Defend_Ratio_foreign <- (foreign_goals_conceded / foreign_games) / avg_local_goals
  
  ratios <- c(Attack_Ratio_local, Defend_Ratio_local,
              Attack_Ratio_foreign, Defend_Ratio_foreign)
  
  
  
  return(ratios)
}

##################################################################################

# Probability_event()
# Inputs: Team ratios & current goal averages.
# Outputs: Probabilities of the following events: Under 4.5 goals, Under 6.5 goals.

Probability_event <- function(local_team_Attack_Ratio_local, 
                              local_team_Defend_Ratio_local, 
                              local_team_Attack_Ratio_foreign, 
                              local_team_Defend_Ratio_foreign,
                              foreign_team_Attack_Ratio_local, 
                              foreign_team_Defend_Ratio_local,
                              foreign_team_Attack_Ratio_foreign, 
                              foreign_team_Defend_Ratio_foreign,
                              avg_local_goals, avg_foreign_goals) {
  
  # Calculation of the current attacking quality rates for the local and foreign team
  local_team_attack <- (local_team_Attack_Ratio_local + 
                          local_team_Attack_Ratio_foreign) / 2
  foreign_team_attack <- (foreign_team_Attack_Ratio_local + 
                            foreign_team_Attack_Ratio_foreign) / 2
  
  
  # Calculation of the current defenssive quality rates for the local and foreign team
  local_team_defend <- (local_team_Defend_Ratio_local+ 
                          local_team_Defend_Ratio_foreign) / 2
  foreign_team_defend <- (foreign_team_Defend_Ratio_local + 
                            foreign_team_Defend_Ratio_foreign) / 2
  
  
  # Calculation of the expected number of goals for the local and foreign team
  local_team_exp <- avg_local_goals * local_team_attack * foreign_team_defend
  foreign_team_exp <- avg_foreign_goals * foreign_team_attack * local_team_defend
  
  # Calculation of the Poisson cummulative distribution function for less than 
  # (lower tail) 4.5 and 6.5 goals
  under4_5 <- ppois(q = 4, lambda = (local_team_exp+foreign_team_exp)/2, 
                    lower.tail = TRUE)
  under6_5 <- ppois(q = 6, lambda = (local_team_exp+foreign_team_exp)/2, 
                    lower.tail = TRUE)
  
  probs <- c(under4_5,under6_5)
  
  return(probs)
  
}

##################################################################################

# Fair_Odds_calculation()
# Inputs: Probabilities of the following events: Under 4.5 goals, Under 6.5 goals. 
# Outputs: Fair Odds of the required events: Under/Over 4.5 goals, Under/Over 6.5 goals.

Fair_Odds_calculation <- function(probability1, probability2) {
  
  oddU45 <- (1/probability1)
  oddU65 <- (1/probability2)
  
  oddO45 <- (probability1/(1-probability1))-1
  oddO65 <- (probability2/(1-probability2))-1
  
  # We cannot offer odds smaller than 1.01
  if (oddU45 <= 1){
    oddU45 <- 1.01
    
    } else if (oddO45<= 1) {
      oddO45 <- 1.01
  } 
  
  if (oddO65 <= 1){
    oddO65 <- 1.01
    
   }  else if (oddU65<= 1) {
    oddU65 <- 1.01
  }
  
  odds <- c(oddU45, oddU65, oddO45, oddO65)
  return(odds)
  
}

##################################################################################

# Tipico_Odds_calculation()
# Inputs: Bookmaker margin, Expected Stakes Over 4.5 goals, 
# Expected Stakes under 4.5 goals.
# Outputs: Odds Over/Under 4.5 goals

Tipico_Odds_calculation <- function(BookmakerMargin, 
                                    ExpectedStakesOver, ExpectedStakesUnder) {
  
  K <- ExpectedStakesOver/ExpectedStakesUnder
  N <- 1+BookmakerMargin
  oddOver <- round((K+1)/(K*N), 2)
  oddUnder <- round(K*oddOver, 2)
  
  # We cannot offer odds smaller than 1.01
  if (oddOver <= 1){
  oddOver <- 1.01
  
    
  } else if (oddUnder<= 1) {
  oddUnder <- 1.01
  
    
  }
  
  odds <- list(oddOver, oddUnder)
  return(odds)
  
}

#c.1
OddsTipico <- Tipico_Odds_calculation(0.1,800,900)
#cat('Odds Over 4.5 goals: ',OddsTipico[[1]],'\n',
#    'Odds Under 4.5 goals:',OddsTipico[[2]])

#c.2
OddsTipico <- Tipico_Odds_calculation(0.2,120,40)
#cat('Odds Over 4.5 goals: ',OddsTipico[[1]],'\n',
#    'Odds Under 4.5 goals:',OddsTipico[[2]])

#c.3
OddsTipico <- Tipico_Odds_calculation(0.04,100,5000)
#cat('Odds Over 4.5 goals: ',OddsTipico[[1]],'\n',
#    'Odds Under 4.5 goals:',OddsTipico[[2]])


##################################################################################
##################################################################################
##
##  The main programme driver  
##
##################################################################################
##################################################################################

# Although loops tend to be notoriously slow in R, it will suffice for 
# prototyping. In production, we would want to vectorize everything and only 
# use apply-family functions (often having time complexity of O(n) or less). If extra
# speed is required, we could compile the code using the {compiler} package.

for(i in 1:nrow(df)){
  # Pull the game statistics from df
  local_team <- as.character(df[i,"Team1"])
  foreign_team <- as.character(df[i,"Team2"])
  local_goals <- as.integer(df[i,"TotScore_T1"])
  foreign_goals <- as.integer(df[i,"TotScore_T2"])
  
  
  # We make sure there is enough historical data to derive 'reliable' results.
  if (teamPerformance[local_team, "local_games"] >2 &&  
      teamPerformance[local_team, "foreign_games"] >2 &&
      teamPerformance[foreign_team, "local_games"] >2 && 
      teamPerformance[foreign_team, "foreign_games"] >2) {
    
    # We estimate the current performance ratios of both teams.
      local_team_Ratios <- Team_ratios_calculation(teamPerformance[local_team, "local_goals"], 
                              teamPerformance[local_team, "local_games"], 
                              teamPerformance[local_team, "avg_local_goals"],
                              teamPerformance[local_team, "local_conceded"], 
                              teamPerformance[local_team, "avg_foreign_goals"], 
                              teamPerformance[local_team, "foreign_goals"], 
                              teamPerformance[local_team, "foreign_games"], 
                              teamPerformance[local_team, "foreign_conceded"])
    
      foreign_team_Ratios <- Team_ratios_calculation(teamPerformance[foreign_team, "local_goals"], 
                              teamPerformance[foreign_team, "local_games"], 
                              teamPerformance[foreign_team, "avg_local_goals"],
                              teamPerformance[foreign_team, "local_conceded"], 
                              teamPerformance[foreign_team, "avg_foreign_goals"], 
                              teamPerformance[foreign_team, "foreign_goals"], 
                              teamPerformance[foreign_team, "foreign_games"], 
                              teamPerformance[foreign_team, "foreign_conceded"])
      
      # We estimate the probabilities for the following events: under 4.5 goals, under 6.5 goals.
      gameProbabilities <- Probability_event(local_team_Ratios[1], local_team_Ratios[2], 
                                             local_team_Ratios[3], local_team_Ratios[4],
                                             foreign_team_Ratios[1], foreign_team_Ratios[2], 
                                             foreign_team_Ratios[3], foreign_team_Ratios[4], 
                                             as.numeric(teamPerformance[local_team, "avg_local_goals"]), 
                                             as.numeric(teamPerformance[foreign_team, "avg_foreign_goals"]))
      
      
      # We estimate the fair Odds of the required events: Under/Over 4.5 goals, Under/Over 6.5 goals.
      gameOdds <- Fair_Odds_calculation(gameProbabilities[1], gameProbabilities[2])
      #print(c("Match number: ",i))
      #print(paste(local_team,' vs ', foreign_team))
      #print(c("Provided U/O 4.5 Odds: ", df[i, "Under4_5"], df[i, "Over4_5"])) 
      #print(c("Computed U/O 4.5 Odds: ", gameOdds[1], gameOdds[2]))
      #print(c("Computed U/O 6.5 Odds: ", gameOdds[3], gameOdds[4]))  
  } 
  
  
  
  
  
  # Finally, we update the Performance Stats for both involved teams.
  teamPerformance[local_team, "local_goals"] <- teamPerformance[local_team, "local_goals"] + 
    local_goals
  teamPerformance[foreign_team, "foreign_goals"] <- teamPerformance[foreign_team, "foreign_goals"] + 
    foreign_goals
  
  teamPerformance[local_team, "local_conceded"] <- teamPerformance[local_team, "local_conceded"] + 
    foreign_goals
  teamPerformance[foreign_team, "foreign_conceded"] <- teamPerformance[foreign_team, "foreign_conceded"] + 
    local_goals 
  
  teamPerformance[local_team, "local_games"] <- teamPerformance[local_team, "local_games"] + 1
  teamPerformance[foreign_team, "foreign_games"] <- teamPerformance[foreign_team, "foreign_games"] + 1
  
  
  teamPerformance[local_team, "avg_local_goals"] <- teamPerformance[local_team, "local_goals"] / 
    (teamPerformance[local_team, "local_games"] +teamPerformance[local_team, "foreign_games"])
  
  teamPerformance[foreign_team, "avg_foreign_goals"] <- teamPerformance[foreign_team, "foreign_goals"] / 
    (teamPerformance[foreign_team, "local_games"]+teamPerformance[foreign_team, "foreign_games"])
  
}
