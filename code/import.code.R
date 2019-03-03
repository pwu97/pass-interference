library(tidyverse)
library(stringr)

allo <- dummy
alld <- dummy
csv_list <- list.files(path="Data", pattern="tracking_gameId_")
csv_list <- csv_list[1]
for (i in csv_list)
{
file.tracking <- paste("Data/",i,sep="")
tracking.example <- read_csv(file.tracking)

file.game <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv"
games.sum <- read_csv(file.game) 

file.plays <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv"
plays.sum <- read_csv(file.plays) 

tracking.example.merged <- tracking.example %>% inner_join(games.sum) %>% inner_join(plays.sum) 

tracking.example.penalty <- filter(tracking.example.merged, isPenalty == TRUE)
penalty.playresult <- tracking.example.penalty[!duplicated(tracking.example.penalty$playId),]

penalty.playresult.defense <- penalty.playresult[grep("Defensive", 
                                              penalty.playresult$playDescription),]
alld <- rbind(penalty.playresult.defense, alld)

penalty.playresult.offense <- penalty.playresult[grep("Offensive", 
                                                      penalty.playresult$playDescription),]
allo <- rbind(penalty.playresult.offense, allo)
}

allo.nopunt <- allo[-grep("punt", allo$playDescription),]
allo.nopunt <- allo.nopunt[-grep("kick", allo.nopunt$playDescription),]

alld.nopunt <- alld[-grep("punt", alld$playDescription),]
alld.nopunt <- alld.nopunt[-grep("kick", alld.nopunt$playDescription),]

ggplot(alld.nopunt, aes(x=PlayResult)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins=18) +
  geom_density(alpha=.2, fill="#FF6666")
  


as.numeric(a$PlayResult)[2000]
summary(tracking.example.penalty$PlayResult)
table(tracking.example.penalty$PlayResult)


example.play <- tracking.example.merged %>% filter(playId == 364)

example.play %>% select(playDescription) %>% slice(1)

allo <- dummy
alld <- dummy
csv_list <- list.files(path="Data", pattern="tracking_gameId_")
csv_list <- csv_list[1]
for (i in csv_list)
{
  file.tracking <- paste("Data/",i,sep="")
  tracking.example <- read_csv(file.tracking)
  
  file.game <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv"
  games.sum <- read_csv(file.game) 
  
  file.plays <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv"
  plays.sum <- read_csv(file.plays) 
  
  tracking.example.merged <- tracking.example %>% inner_join(games.sum) %>% inner_join(plays.sum) 
  
  pass.incomplete.1 <- tracking.example.merged[grep("pass incomplete short right to F.Gore", 
                                                             tracking.example.merged$playDescription),]
}
