matches <- read.csv(file = "WorldCupMatches.csv", stringsAsFactors = FALSE,header=TRUE)
players <- read.csv(file ="WorldCupPlayers.csv",stringsAsFactors = FALSE,header=TRUE)
worldcups <- read.csv(file ="WorldCups.csv",stringsAsFactors = FALSE ,header=TRUE)


## exploring raw data
names(matches)
summary(matches)
tyhead(matches)

names(players)
summary(players)
head(players)

names(worldcups)
summary(worldcups)
head(worldcups)

library(ggplot2)
library(DataExplorer)
plot_str(worldcups)
plot_histogram(worldcups)
plot_bar(worldcups) 

# converting to an int

worldcups$Year  <- as.numeric(as.character(worldcups$Year))

worldcups$Attendance  <- as.numeric(worldcups$Attendance)
typeof(worldcups$Attendance)


##  checking for NA's and then cleaning data 

sum(is.na(matches$Home.Team.Name))
(is.na(matches$Home.Team.Goals))

sum(is.na(matches$Away.Team.Name))
sum(is.na(matches$Away.Team.Goals))
matches<-  na.omit(matches)
library(VIM)
##library(easyGgplot2)
#matches1 <- kNN(matches, variable = c("Home.Team.Goals", "Away.Team.Goals","Attendance"), k = 5)
#matches1 <- kNN(matches, variable = c("Home.Team.Goals", "Away.Team.Goals","Attendance"), k = 10)
#matches1 <- kNN(matches, variable = c("Home.Team.Goals", "Away.Team.Goals","Attendance"), k = 15)
#matches1 <- kNN(matches, variable = c("Home.Team.Goals", "Away.Team.Goals","Attendance"), k = 20)
#matches1 <- kNN(matches, variable = c("Home.Team.Goals", "Away.Team.Goals","Attendance"), k = 50)
matches1 <- kNN(matches, variable = c("Home.Team.Goals", "Away.Team.Goals","Attendance"), k = 100)
summary(matches1)
head(matches1)

# display number of goals at home
ggplot(data = matches1) +
  geom_bar(mapping = aes(reorder(Home.Team.Goals,Home.Team.Goals, length)))+ xlab("number of goals scored at home")+ coord_flip()

# display number of goals away
ggplot(data = matches1) +
  geom_bar(mapping = aes(reorder(Away.Team.Goals,Away.Team.Goals, length)))+ xlab("number of goals scored away")

#

# group stages
ggplot(data = matches1) + geom_bar(mapping = aes(reorder(Stage,Stage, length)))+ xlab("group stages")+ coord_flip()


# winners
ggplot(data = worldcups) + geom_bar(mapping = aes(reorder(Winner,Winner, length)))+ xlab("winners")+ coord_flip()

# home goals
home_goal <- ggplot(matches1, aes(x=Home.Team.Name, y=Home.Team.Goals)) + geom_bar(stat = "identity", aes(fill=Home.Team.Name))
print(home_goal)

#away goals
away_goal <- ggplot(matches1, aes(x=Away.Team.Name, y=Away.Team.Goals)) + geom_bar(stat = "identity", aes(fill=Home.Team.Name))
print(away_goal)

#winner vs attendance
winner_attendance <- ggplot(worldcups, aes(x=Winner, y= Attendance )) + geom_bar(stat = "identity", aes(fill=Winner))
print(winner_attendance)

# year and attendance 
gg_bar <- ggplot(worldcups, aes(x=Year, y= Attendance )) + geom_bar(stat = "identity", aes(fill=Year))
print(gg_bar)

# converting to an int

worldcups$Year  <- as.numeric(as.character(worldcups$Year))


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




