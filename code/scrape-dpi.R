## Scrape DPI data, similar to `generate-testnew.R`

file.game <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv"
file.plays <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv"

csv_list <- list.files(path= "Data", pattern="tracking_gameId_")

games_dpi <- c()

for (i in 1:91)
{
  file.tracking <- paste("Data/", csv_list[i], sep="")
  tracking.example <- read_csv(file.tracking)
    
  games.sum <- read_csv(file.game) 
    
  plays.sum <- read_csv(file.plays) 
    
  tracking.example.merged <- tracking.example %>% inner_join(games.sum) %>% inner_join(plays.sum) 
    
  dpi <- tracking.example.merged[grep("Defensive Pass Interference", tracking.example.merged$playDescription),]
  
  if (nrow(dpi) != 0)
  {
    games_dpi <- c(games_dpi, i)
  }
}

#games_dpi has the games with a DPI

dpi_rows <- data.frame()

for(k in i)
{
  file.tracking <- paste("Data/", csv_list[i], sep="")
  tracking.example <- read_csv(file.tracking)
  
  games.sum <- read_csv(file.game) 
  
  plays.sum <- read_csv(file.plays) 
  
  tracking.example.merged <- tracking.example %>% inner_join(games.sum) %>% inner_join(plays.sum) 

  dpi <- tracking.example.merged[grep("Defensive Pass Interference", tracking.example.merged$playDescription),]
  
  example.play <- tracking.example.merged %>% filter(playId == dpi$playId[1])
  
  example.play %>% select(playDescription) %>% slice(1)
  
  dpi_rows <- rbind(dpi_rows, example.play)
}
