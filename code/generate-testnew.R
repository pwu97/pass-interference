library(tidyverse)
library(stringr)
library(readr)
library(stringr)

## Target data frame for data
testnew <- data.frame()

## Proceed to scrape the Github files for predictors
csv_list <- list.files(path="Data", pattern="tracking_gameId_")
csv_list <- csv_list[1:2]
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
  
  #Get pass forward and pass arrive events for both teams
  tracking.example.pf1 <- filter(tracking.example.merged, event == "pass_forward")
  tracking.example.pf2 <- filter(tracking.example.merged, event == "pass_arrived")
  tracking.example.pf <- rbind(tracking.example.pf1, tracking.example.pf2)
  
  #tracking.example.pf <- filter(tracking.example.pf, event == "pass_forward")
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
    examine4 <- example.play[c(1:22),]
    
    examine1 <- example.play[grep(example.play$Thrower[1], example.play$displayName),]
    
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
    examine2 <- filter(example.play, displayName == example.play$displayName[k])
    examine3 <- filter(examine3, team != examine2$team[1])
    examine4 <- filter(examine4, team != examine2$team[1])
    examine <- rbind(examine1, examine2, examine3, examine4)
    
    pr_min <- transmute(examine, pr_min = min( 
      sqrt((x[1]-x[16])^2+(y[1]-y[16])^2),
      sqrt((x[1]-x[17])^2+(y[1]-y[17])^2),
      sqrt((x[1]-x[18])^2+(y[1]-y[18])^2),
      sqrt((x[1]-x[19])^2+(y[1]-y[19])^2),
      sqrt((x[1]-x[20])^2+(y[1]-y[20])^2),
      sqrt((x[1]-x[21])^2+(y[1]-y[21])^2),
      sqrt((x[1]-x[22])^2+(y[1]-y[22])^2),
      sqrt((x[1]-x[23])^2+(y[1]-y[23])^2),
      sqrt((x[1]-x[24])^2+(y[1]-y[24])^2),
      sqrt((x[1]-x[25])^2+(y[1]-y[25])^2),
      sqrt((x[1]-x[26])^2+(y[1]-y[26])^2)))
    
    pr_min <- pr_min[1,]
    pr_min
    
    pa_min <- transmute(examine, pa_min = min( 
      sqrt((x[4]-x[5])^2+(y[4]-y[5])^2),
      sqrt((x[4]-x[6])^2+(y[4]-y[6])^2),
      sqrt((x[4]-x[7])^2+(y[4]-y[7])^2),
      sqrt((x[4]-x[8])^2+(y[4]-y[8])^2),
      sqrt((x[4]-x[9])^2+(y[4]-y[9])^2),
      sqrt((x[4]-x[10])^2+(y[4]-y[10])^2),
      sqrt((x[4]-x[11])^2+(y[4]-y[11])^2),
      sqrt((x[4]-x[12])^2+(y[4]-y[12])^2),
      sqrt((x[4]-x[13])^2+(y[4]-y[13])^2),
      sqrt((x[4]-x[14])^2+(y[4]-y[14])^2),
      sqrt((x[4]-x[15])^2+(y[4]-y[15])^2)))
    
    pa_min <- pa_min[1,]
    pa_min
    
    passlength <- transmute(examine, passlength = abs(PassLength))
    passlength <- passlength[1, ]
    passlength
    
    air_distance <- transmute(examine, air_distance = sqrt((x[1]-x[4])^2+(y[1]-y[4])^2))
    air_distance <- air_distance[1,]
    air_distance
    sideline_distance <- transmute(examine, sideline_distance = min(1.0*(53.3-y[2]), y[2]))
    sideline_distance <- sideline_distance[1,]
    time_to_throw <- transmute(examine, time_to_throw = frame.id[1]/10)
    time_to_throw <- time_to_throw[1,]
    time_to_throw
    passer_speed <- transmute(examine,passer_speed=s[1])
    passer_speed <- passer_speed[1,]
    passer_speed
    completion <- examine$Completion[1]
    play_result <- examine$PlayResult[1]
    yards_after_catch <- examine$YardsAfterCatch[1]
    play_desc <- examine$playDescription[1]
    penalty_var <- examine$isPenalty[1]
    
    a <- cbind(air_distance,sideline_distance,time_to_throw,passer_speed,
               play_result,yards_after_catch, play_desc, penalty_var, pa_min, pr_min, passlength)
    a <- cbind(a,completion)
    testnew <- rbind(a, testnew)
  }
}

a <- na.omit(testnew$air_distance)

a <- data.frame(glm.probs)

## Simple plot for one predictor
ggplot(data=a, aes(x=glm.probs)) + 
  geom_histogram(aes(y=stat(density)), bins=50, color="black",fill="white") + 
  geom_density(color="blue") +
  theme_bw() +
  xlab("Air Distance") +
  ylab("Density") +
  ggtitle("Distribution of Air Distance on Catch Plays (91 Games)")

## Histogram for a predictor
hist(a, breaks=75, xlim = c(0,60), ylim=c(0,0.1))
lines(seq(0,60), dnorm(seq(0,60), mean(a), sd(a)),
      col = 'dodgerblue', lwd = 4)

## Save as csv
write_csv(testnew, '/Users/peter/Downloads/Big-Data-Bowl-master/Plots/all-catch-pass-plays-pa-pr.csv')
