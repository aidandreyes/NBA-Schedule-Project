# Setup and Data
library(plotly)
library(tidyverse)

schedule <- read_csv("schedule.csv")
draft_schedule <- read_csv("schedule_24_partial.csv")
locations <- read_csv("locations.csv")
game_data <- read_csv("team_game_data.csv")
