## Not used
## Code was to filter by type of penalty
## and then test out the grep() function

library(tidyverse)
library(stringr)

file.tracking <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017091000.csv"
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

penalty.playresult.offense <- penalty.playresult[grep("Offensive", 
                                                      penalty.playresult$playDescription),]