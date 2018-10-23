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
library(stringr)
matches_clean$WonHome <- ifelse(matches_clean$Home.Team.Goals > matches_clean$Away.Team.Goals,1,0)
matches_clean$WonAway <- ifelse(matches_clean$Away.Team.Goals > matches_clean$Home.Team.Goals,1,0)
matches_clean$team_won <- ifelse(matches_clean$Home.Team.Goals > matches_clean$Away.Team.Goals,matches_clean$Home.Team.Initials,matches_clean$Away.Team.Initials)
mutate(matches_clean, goal_diff = Home.Team.Goals - Away.Team.Goals)


typeof(players$Event)

remove_spaces <- str_replace_all(string=players$Event, pattern=" ", repl="")
remove_char <- str_replace_all(remove_spaces,"[[:punct:]]", "")
remove_char
players$Event_transform<- gsub("[^a-zA-Z]", "", remove_char)
players$Event_transform
#players$Num.goals <- ifelse(players$Event_transform == "G",1,
                       #ifelse(players$Event_transform == "GG",2, 
                        # ifelse(players$Event_transform == "GGG",3,
                         # ifelse(players$Event_transform == "PG",2,
                          #  ifelse(players$Event_transform == "P",1,)

#event_df <- c(players$Event_transform)
#event_df

#eventCount <- c()

#for(event in 1:event_df){
  #if(event[i] == "G"){
   # eventCount <- c(eventCount,1)
    
  #}else if(event[i] == "GG"){
    
   # eventCount <- c(eventCount,2)
    
  #}else if(event[i] == "GGG"){
    
   # eventCount <- c(eventCount,3)
 # }else if(event[i] == "PG"){
    
    #eventCount <- c(eventCount,2)
    
 # }else if(event[i] == "P"){
    
   # eventCount <- c(eventCount,1)
 # }
#}
 
#players$Num.goals <- eventCount
#players$Num.goals


event_df <- c(players$Event_transform)
eventCount <- c()

for(i in 1:dim(players)[1]){
  
  if(players$Event_transform[i] == 'G'){
    players$Num.goals[i] <- 1
  
  }else if(players$Event_transform[i] == 'GG'){
  
    players$Num.goals[i] <- 2
  
  }else if(players$Event_transform[i] == 'GGG'){
    players$Num.goals[i] <- 3
   
   }else if(players$Event_transform[i] == 'PG'){
  
     players$Num.goals[i] <- 2
  
   }else if(players$Event_transform[i] == 'P'){
     players$Num.goals[i] <- 1
   
   }else if(players$Event_transform[i] == 'IH'){
    players$Num.goals[i] <- 0
    
  } else if(players$Event_transform[i] == '' ){
    players$Num.goals[i] <- 0
    
  }
  
}
players$Num.goals


library(tidyr)









