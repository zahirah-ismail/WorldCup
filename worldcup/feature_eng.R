matches <- read.csv(file = "WorldCupMatches.csv", stringsAsFactors = FALSE,header=TRUE)
players <- read.csv(file ="WorldCupPlayers.csv",stringsAsFactors = FALSE,header=TRUE)
worldcups <- read.table(file ="WorldCups.csv",stringsAsFactors = FALSE ,header=TRUE ,sep = ",", dec = ".")


# converting to years to int

worldcups$Year  <- as.numeric(as.character(worldcups$Year))


#converting to int 

worldcups$Attendance  <- as.numeric(worldcups$Attendance)
typeof(worldcups$Attendance)


##  checking for NA's and then cleaning data 

sum(is.na(matches$Home.Team.Name))
sum(is.na(matches$Home.Team.Goals))

sum(is.na(matches$Away.Team.Name))
sum(is.na(matches$Away.Team.Goals))


#removed all the NA rows

matches_clean <- na.omit(matches)
matches_clean

sum(is.na(worldcups$Attendance))

## Feature engineering
library(dplyr)
matches_clean$WonHome <- ifelse(matches_clean$Home.Team.Goals > matches_clean$Away.Team.Goals,1,0)
matches_clean$WonAway <- ifelse(matches_clean$Away.Team.Goals > matches_clean$Home.Team.Goals,1,0)
matches_clean$team_won <- ifelse(matches_clean$Home.Team.Goals > matches_clean$Away.Team.Goals,matches_clean$Home.Team.Initials,matches_clean$Away.Team.Initials)
mutate(matches_clean, goal_diff = Home.Team.Goals - Away.Team.Goals)