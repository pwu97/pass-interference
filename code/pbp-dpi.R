library(magrittr)

pbp <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2017.csv")

dpi <- 
  pbp %>%
  filter(game_date < as.Date("2017-10-19")) %>%
  filter(penalty_type == "Defensive Pass Interference")

all.pass.plays
