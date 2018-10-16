matches <- read.csv(file = "WorldCupMatches.csv", stringsAsFactors = FALSE,header=TRUE)
players <- read.csv(file ="WorldCupPlayers.csv",stringsAsFactors = FALSE,header=TRUE)
worldcups <- read.table(file ="WorldCups.csv",stringsAsFactors = FALSE ,header=TRUE ,sep = ",", dec = ".")

# read.table()

## exploring raw data
names(matches)
summary(matches)
head(matches)
str(matches)

names(players)
summary(players)
head(players)

names(worldcups)
summary(worldcups)
head(worldcups)
worldcups

library(ggplot2)
library(DataExplorer)
plot_str(worldcups)
plot_histogram(worldcups)
plot_bar(worldcups) 

plot_str(matches)
plot_histogram(matches)
plot_bar(matches) 

plot_str(players)
plot_histogram(players)
plot_bar(players) 
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

#worldcups1 <- kNN(worldcups, variable = c("Attendance"), k = 8)
#worldcups1

library(VIM)
?VIM
#matches1 <- kNN(matches_clean, variable = c("Home.Team.Goals", "Away.Team.Goals","Attendance"), k = 100)
#summary(matches1)
#head(matches1)

# display number of goals at home
ggplot(data = matches_clean) +
  geom_bar(mapping = aes(reorder(Home.Team.Goals,Home.Team.Goals, length)))+ xlab("number of goals scored at home")+ coord_flip()

# display number of goals away
ggplot(data = matches_clean) +
  geom_bar(mapping = aes(reorder(Away.Team.Goals,Away.Team.Goals, length)))+ xlab("number of goals scored away")

#

# group stages
ggplot(data = matches_clean) + geom_bar(mapping = aes(reorder(Stage,Stage, length)))+ xlab("group stages")+ coord_flip()


# winners
ggplot(data = worldcups) + geom_bar(mapping = aes(reorder(Winner,Winner, length)))+ xlab("winners")+ coord_flip()

# home goals
#home_goal <- ggplot(matches1, aes(x=Home.Team.Name, y=Home.Team.Goals)) #+ geom_bar(stat = "identity", aes(fill=Home.Team.Name))+ coord_flip()
#print(home_goal)

#away goals
#away_goal <- ggplot(matches1, aes(x=Away.Team.Name, y=Away.Team.Goals)) #+ geom_bar(stat = "identity", aes(fill=Home.Team.Name))+ coord_flip()
#print(away_goal)

#winner vs attendance
winner_attendance <- ggplot(worldcups, aes(x=Winner, y= Attendance )) + geom_bar(stat = "identity", aes(fill=Winner))
print(winner_attendance)

# year and attendance 
year_attendance <- ggplot(worldcups, aes(x=Year, y= Attendance )) + geom_bar(stat = "identity", aes(fill=Year))
print(year_attendance)




# plot of attendance vs year

ggplot(worldcups, aes(Year, Attendance)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", 
       y = "attendeance ", 
       title = "attendance in each year")+ theme(plot.title = element_text(hjust = 0.5))  

# runner up

ggplot(data = worldcups) + geom_bar(mapping = aes(reorder( Runners.Up ,Runners.Up , length)))+ xlab("Runners.Up")


##winner vs match played
winner_matchPlayed <- ggplot(worldcups, aes(x=Winner, y= MatchesPlayed )) + geom_bar(stat = "identity", aes(fill=Winner))
print(winner_matchPlayed)

## Feature engineering

library(dplyr)
matches_clean$WonHome <- ifelse(matches_clean$Home.Team.Goals > matches_clean$Away.Team.Goals,1,0)
matches_clean$WonAway <- ifelse(matches_clean$Away.Team.Goals > matches_clean$Home.Team.Goals,1,0)
matches_clean$team_won <- ifelse(matches_clean$Home.Team.Goals > matches_clean$Away.Team.Goals,matches_clean$Home.Team.Initials,matches_clean$Away.Team.Initials)
mutate(matches_clean, goal_diff = Home.Team.Goals - Away.Team.Goals)

#top_scorer <- players %>% add_count(Player.Name, sort = TRUE) %>% filter()
#top_scorer


## new CSV for players playing in worldcups
#typeof(players$Player.Name)
#player <- data.frame(select(players,Player.Name))
#player

#for (i in player){
  ##playername <- playersPlayer.Name == players$Player.Name[i]
  #playercount <- length(player[i]==TRUE)
  #print(playercount)
#}

table(players$Player.Name)

require(sqldf)

referee_1930 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 1930 AND "Home.Team.Initials" = "URU" OR "Away.Team.Initials" = "URU"')
table(referee_1930)

referee_1938 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 1938 AND "Home.Team.Initials" = "ITA" OR "Away.Team.Initials" = "ITA"')
table(referee_1938)

referee_1950 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 1950 AND "Home.Team.Initials" = "ITA" OR "Away.Team.Initials" = "ITA"')
table(referee_1950)

referee_1954 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 1954 AND "Home.Team.Initials" = "URU" OR "Away.Team.Initials" = "URU"')
table(referee_1954)

referee_1958 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 1958 AND "Home.Team.Initials" = "GER" OR "Away.Team.Initials" = "GER"')
table(referee_1958)

referee_1962 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 1962 AND "Home.Team.Initials" = "BRA" OR "Away.Team.Initials" = "BRA"')
table(referee_1962)

referee_1966 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 1966 AND "Home.Team.Initials" = "ENG" OR "Away.Team.Initials" = "ENG"')
table(referee_1966)

referee_1970 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 1970 AND "Home.Team.Initials" = "BRA" OR "Away.Team.Initials" = "BRA"')
table(referee_1970)


referee_1974 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 1974 AND "Home.Team.Initials" = "GER" OR "Away.Team.Initials" = "GERM"')
table(referee_1974)

referee_1978 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 1978 AND "Home.Team.Initials" = "ARG" OR "Away.Team.Initials" = "ARG"')
table(referee_1978)

referee_1982 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 1982 AND "Home.Team.Initials" = "ITA" OR "Away.Team.Initials" = "ITA"')
table(referee_1982)

referee_1986 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 1986 AND "Home.Team.Initials" = "ARG" OR "Away.Team.Initials" = "ARG"')
table(referee_1986)

referee_1990 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 1990 AND "Home.Team.Initials" = "GER" OR "Away.Team.Initials" = "GER"')
table(referee_1990)

referee_1994 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 1994 AND "Home.Team.Initials" = "BRA" OR "Away.Team.Initials" = "BRA"')
table(referee_1994)

referee_1998 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 1998 AND "Home.Team.Initials" = "FRA" OR "Away.Team.Initials" = "FRA"')
table(referee_1998)

referee_2002 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 2002 AND "Home.Team.Initials" = "BRA" OR "Away.Team.Initials" = "BRA"')
table(referee_2002)

referee_2006 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 2006 AND "Home.Team.Initials" = "ITA" OR "Away.Team.Initials" = "ITA"')
table(referee_2006)

referee_2010 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 2010 AND "Home.Team.Initials" = "ESP" OR "Away.Team.Initials" = "ESP"')
table(referee_2010 )

referee_2014 <- sqldf('SELECT Referee FROM  matches_clean WHERE Year = 2014 AND "Home.Team.Initials" = "GER" OR "Away.Team.Initials" = "GER"')
table(referee_2014)

player_name <- sqldf('SELECT "Player.Name" From players WHERE Event <> ""')
table(player_name) 

