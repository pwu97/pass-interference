## Get data for all catch plays, similar to `generate-testnew.R`

## Used to edit `generate-testnew.R` code while still keeping
## original file in tact

library(tidyverse)
library(stringr)
library(readr)
library(stringr)

testnew <- data.frame()

csv_list <- list.files(path="Data", pattern="tracking_gameId_")
i <- csv_list[4]
csv_list

for (i in csv_list)
{
  file.tracking <- paste("Data/", i, sep="")
  tracking.example <- read_csv(file.tracking)
  
  file.game <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv"
  games.sum <- read_csv(file.game) 
  
  file.plays <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv"
  plays.sum <- read_csv(file.plays) 
  
  tracking.example.merged <- tracking.example %>% inner_join(games.sum) %>% inner_join(plays.sum) 
  
  tracking.example.pf <- filter(tracking.example.merged, isPenalty == TRUE)
  #tracking.example.pf <- filter(tracking.example.pf, event == "pass_forward")
  #tracking.example.t <- tracking.example.pf[grep("Defensive Holding", tracking.example.pf$playDescription),]
  tracking.example.pf.d <- tracking.example.pf[grep("Defensive", tracking.example.pf$playDescription),]
  tracking.example.pf.o <- tracking.example.pf[grep("Offensive", tracking.example.pf$playDescription),]
  tracking.example.pf <- rbind(tracking.example.pf.d, tracking.example.pf.o)
  # tracking.example.pf[!is.na(tracking.example.pf$PlayResult),]
  
  splitted <- split(tracking.example.pf, f = tracking.example.pf$playId)
  
  j <- length(splitted)
  
  for (j in 1:j)
  {
    examine1 <- data.frame()
    example.play <- splitted[[j]]
    
    examine1 <- example.play[grep(example.play$Thrower[1], example.play$Thrower[1]),]
    
    k <- 1
    repeat{
      examine2 <- example.play[grep(example.play$displayName[k], example.play$Receiver[1]),]
      k = k + 1
      if (nrow(examine2) == 1)
      {
        break
      }
    }
    k = k - 1
    examine2 <- example.play[c(k),]
    examine <- rbind(examine1, examine2)
    
    air_distance <- transmute(examine, air_distance = abs(PassLength))
    air_distance <- air_distance[-1,]
    air_distance
    sideline_distance <- transmute(examine, sideline_distance = min(1.0*(53.3-y[2]), y[2]))
    sideline_distance <- sideline_distance[-1,]
    time_to_throw <- transmute(examine, time_to_throw = frame.id[1]/10)
    time_to_throw <- time_to_throw[-1,]
    time_to_throw
    passer_speed <- transmute(examine,passer_speed=s[1])
    passer_speed <- passer_speed[-1,]
    passer_speed
    completion <- examine$Completion[1]
    play_result <- examine$PlayResult[1]
    yards_after_catch <- examine$YardsAfterCatch[1]
    play_desc <- examine$playDescription[1]
    
    a <- cbind(air_distance,sideline_distance,time_to_throw,passer_speed,
               play_result,yards_after_catch, play_desc)
    a <- cbind(a,completion)
    testnew <- rbind(a, testnew)
  }
}

write_csv(testnew, '/Users/peter/Downloads/Big-Data-Bowl-master/Plots/all-pass-plays.csv')
