## Code to check filtering by penalties

library(dplyr)

file.tracking.penalty <- filter(file.tracking, isPenalty == TRUE)