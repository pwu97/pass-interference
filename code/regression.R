## Code similar to `generate-testnew.R`

library(tidyverse)
library(stringr)
library(readr)
library(stringr)

testnew <- data.frame()

csv_list <- list.files(path="Data", pattern="tracking_gameId_")
csv_list <- csv_list[1:91]
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

  tracking.example.pf <- filter(tracking.example.merged, event == "pass_forward")
  #tracking.example.pf <- tracking.example.pf[grep("pass", tracking.example.pf$playDescription),]
  #tracking.example.pf.d <- tracking.example.pf[grep("Defensive", tracking.example.pf$playDescription),]
  #tracking.example.pf.o <- tracking.example.pf[grep("Offensive", tracking.example.pf$playDescription),]
  #tracking.example.pf <- rbind(tracking.example.pf.d, tracking.example.pf.o)
  tracking.example.pf[!is.na(tracking.example.pf$PlayResult),]
  
  i <- nrow(tracking.example.pf)
  if (i == 0)
  {
    next
  }
  
  tracking.example.pf[,c("Completion")] <- NA
  for (i in 1:i)
  {
    if (tracking.example.pf$PlayResult[i] > 0)
    {
      tracking.example.pf$Completion[i] <- 1
    }
    else
    {
      tracking.example.pf$Completion[i] <- 0
    }
  }
  
  tracking.example.pf$displayName <- word(tracking.example.pf$displayName, -1)
  
  x <- sub(' pass.*','',tracking.example.pf$playDescription)
  x <- sub('.*) ','', x)
  x <- sub('.*\\.','',x)
  x
  
  y <- sub(' to ',' hi ',tracking.example.pf$playDescription)
  y <- sub('.* hi ','',y)
  y <- sub('.\\.',' hi ', y)
  y <- sub('.* hi ', '', y)
  y <- sub('\\(.*','',y)
  y <- sub('\\[.*','',y)
  y <- sub(' to .*','',y)
  y <- sub(' ran .*','',y)
  y <- sub(' pushed .*','',y)
  y <- sub(' for .*','',y)
  y <- sub(' pass .*','',y)
  y <- sub('.','',y,fixed=TRUE)
  y

  #tracking.example.pf$Thrower <- NULL
  #tracking.example.pf$Receiver <- NULL
  
  tracking.example.pf$Thrower <- x
  tracking.example.pf$Receiver <- y
  
  splitted <- split(tracking.example.pf, f = tracking.example.pf$playId)
  
  j <- length(splitted)
  
  for (j in 1:j)
  {
    examine1 <- data.frame()
    examine2 <- data.frame()
    examine <- data.frame()
    example.play <- splitted[[j]]
    examine3 <- example.play[c(23:44),]
    
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
    examine2$team[1]
    examine3 <- filter(examine3, team != examine2$team[1])
    examine <- rbind(examine1, examine2, examine3)
    
    examine$x[7]
    
    pass_forward_min <- min()
    
    penalty_var <- examine$isPenalty[1]
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
    penalty_var <- examine$isPenalty[1]
    
    a <- cbind(air_distance,sideline_distance,time_to_throw,passer_speed,
               play_result,yards_after_catch, play_desc, penalty_var)
    a <- cbind(a,completion)
    testnew <- rbind(a, testnew)
  }
}

write_csv(testnew, '/Users/peter/Downloads/Big-Data-Bowl-master/Plots/all-pass-plays.csv')
