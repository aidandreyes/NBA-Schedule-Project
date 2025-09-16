# Setup and Data
library(plotly)
library(tidyverse)

schedule <- read_csv("schedule.csv")
draft_schedule <- read_csv("schedule_24_partial.csv")
locations <- read_csv("locations.csv")
game_data <- read_csv("team_game_data.csv")

# Part 1 Schedule Analysis
# Question 1
# How many games are the 4th game played over the past 6 nights?
draft_schedule_df <- as.data.frame(draft_schedule) # Convert to data frame
okc_2024 <- draft_schedule_df[draft_schedule_df$team == "OKC",] # OKC schedule data frame

okc_2024 <- okc_2024 %>%
  arrange(gamedate) # arrange date 

count <- 0 # count for how many times Thunder will play 4 games in 6 nights

for (i in seq(1, nrow(okc_2024)-3)) { # stops at nrow - 3 so the loop doesn't go out of bounds
  first_game <- okc_2024$gamedate[i] 
  fourth_game <- okc_2024$gamedate[i+3]
  
  days_since_previousgame <- fourth_game - first_game # Stores difference in days
  
  if (days_since_previousgame == 5) {
    count <- count + 1  # Increases every time there is a 4 in 6 stretch
  }
  
}

paste("There are", count, "times this season that the Thunder will play 4 games in 6 nights. ")


# Question 2
# From 2014-15 to 2023-24, what is the average number of 4-in-6 stretches for a team in a season? Adjust each team/season to per-82 games before taking your final average.

schedule_df <- as.data.frame(schedule)
schedule_df <- schedule_df %>% arrange(team, season, gamedate)

teams <- unique(schedule_df$team)

team_season_count <- data.frame(team = character(), season = numeric(), number_of_4in6 = numeric(), stringsAsFactors = FALSE)

# Loop through each team
for (team_now in teams) {

  specific_team <- schedule_df %>%
    filter(team == team_now)

  seasons <- unique(specific_team$season)

  # Loop through each season, for each team
  for (season_now in seasons) {

    # team's schedule for that specific season
    specific_season <- specific_team %>%
      filter(season == season_now) %>%
      arrange(gamedate) 

    count <- 0 # counts how many 4-in-6 stretches

    if (nrow(specific_season) >= 4) {
      for (i in seq(1, nrow(specific_season) -3)) { # 4-in-6 loop (I used the same one as Question 1)
        first_game <- specific_season$gamedate[i]
        fourth_game <- specific_season$gamedate[i+3]
        days_since_previousgame <- fourth_game - first_game

        if (days_since_previousgame == 5) {
          count <- count + 1
        }
      }
    }

    # Main dataframe
    team_season_count <- rbind(team_season_count, data.frame(team = team_now, season = season_now, number_of_4in6 = count))
    
  }
}

# Create games_played list so that it can be added as a new column to the main dataframe
games_played <- schedule_df %>%
  group_by(team, season) %>%
  summarize(games_played = n())

team_season_count <- cbind(team_season_count, games_played$games_played) # combine games played column to compute per 82
team_season_count <- team_season_count %>% 
  mutate(per_82_4in6 = (number_of_4in6 / games_played$games_played) * 82) # per82 takes into account the shortened 2019 & 2020 seasons

new_team_season_count <- team_season_count %>%    # new dataframe to showcase how many 4-in-6 stretches per season from 2014-2023
  group_by(team) %>%
  summarize(per_season_4in6 = mean(per_82_4in6)) # variable per_season_4in6 tells how many 4-in-6 stretches per season for that specific team

mean(new_team_season_count$per_season_4in6) # gives average of how many 4-in-6 stretches a team plays per season

# Question 3
# Which of the 30 NBA teams has had the highest average number of 4-in-6 stretches between 2014-15 and 2023-24? Which team has had the lowest average? Adjust each team/season to per-82 games.

schedule_df <- as.data.frame(schedule)
schedule_df <- schedule_df %>% arrange(team, season, gamedate)

teams <- unique(schedule_df$team)

team_season_count <- data.frame(team = character(), season = numeric(), number_of_4in6 = numeric(), stringsAsFactors = FALSE)

# Loop through each team
for (team_now in teams) {

  specific_team <- schedule_df %>%
    filter(team == team_now)

  seasons <- unique(specific_team$season)

  # Loop through each season, for each team
  for (season_now in seasons) {

    # team's schedule for that specific season
    specific_season <- specific_team %>%
      filter(season == season_now) %>%
      arrange(gamedate) 

    count <- 0 # counts how many 4-in-6 stretches

    if (nrow(specific_season) >= 4) {
      for (i in seq(1, nrow(specific_season) -3)) { # 4-in-6 loop (I used the same one as Question 1)
        first_game <- specific_season$gamedate[i]
        fourth_game <- specific_season$gamedate[i+3]
        days_since_previousgame <- fourth_game - first_game

        if (days_since_previousgame == 5) {
          count <- count + 1
        }
      }
    }

    # Main dataframe
    team_season_count <- rbind(team_season_count, data.frame(team = team_now, season = season_now, number_of_4in6 = count))
    
  }
}

# Create games_played list so that it can be added as a new column to the main dataframe
games_played <- schedule_df %>%
  group_by(team, season) %>%
  summarize(games_played = n())

team_season_count <- cbind(team_season_count, games_played$games_played) # combine games played column to compute per 82
team_season_count <- team_season_count %>% 
  mutate(per_82_4in6 = (number_of_4in6 / games_played$games_played) * 82) # per82 takes into account the shortened 2019 & 2020 seasons

new_team_season_count <- team_season_count %>%    # new dataframe to showcase how many 4-in-6 stretches per season from 2014-2023
  group_by(team) %>%
  summarize(per_season_4in6 = mean(per_82_4in6)) # variable per_season_4in6 tells how many 4-in-6 stretches per season for that specific team

new_team_season_count <- as.data.frame(new_team_season_count)

new_team_season_count[which.max(new_team_season_count$per_season_4in6), ] # extract row of team that has the most 4-in-6 stretches per season
new_team_season_count[which.min(new_team_season_count$per_season_4in6), ] # extract row of team that has the least 4-in-6 stretches per season

# Question 4 in markdown file

# Question 5
# What was BKN’s defensive eFG% in the 2023-24 season? What was their defensive eFG% that season in situations where their opponent was on the second night of back-to-back?

# Defensive eFG% = (field goals made + 0.5 * 3-pointers made) / field goal attempts

game_data_df <- as.data.frame(game_data) # Convert to data frame
bkn <- game_data_df[game_data_df$def_team == "BKN", ] # Brooklyn defense only
bkn_2023 <- bkn[bkn$season == 2023, ]

fgm <- sum(bkn_2023$fgmade) # Total field goals allowed by Brooklyn in 2023-24
fga <- sum(bkn_2023$fgattempted) # Total field goals allowed by Brooklyn in 2023-24
fgm3 <- sum(bkn_2023$fg3made) # Total 3 pointers allowed by Brooklyn in 2023-24

bkn_def_efg <- (fgm + (0.5 * fgm3)) / (fga)
paste("Brooklyn Defensive eFG%: ", bkn_def_efg)

# Defensive effective field goal percentage when opponent was on second night of back-to-back
df_2023 <- game_data_df[game_data_df$season == 2023, ] # new dataframe for filtering: 2023-24 games only

df_2023 <- df_2023%>%arrange(off_team, gamedate) # arrange by team, in order of date

df_2023 <- df_2023 %>%
  group_by(off_team) %>%
  mutate(days_since_previousgame = gamedate - lag(gamedate, default = first(gamedate))) %>% # new variable for days since previous game for off_team
  mutate(off_team_b2b = ifelse(days_since_previousgame == 1, TRUE, FALSE)) # new boolean that determines if game is back-to-back 

bkn_2023 <- df_2023[df_2023$def_team == "BKN", ] # change dataframe to now have new variables and also grouped
bkn_opp_b2b <- bkn_2023[bkn_2023$off_team_b2b == TRUE, ]

b2b_fgm <- sum(bkn_opp_b2b$fgmade) # Total field goals allowed
b2b_fga <- sum(bkn_opp_b2b$fgattempted) # Total field goal attempts
b2b_fgm3 <- sum(bkn_opp_b2b$fg3made) # Total 3-pointers allowed

bkn_def_efg_b2b <- (b2b_fgm + (0.5 * b2b_fgm3)) / (b2b_fga)
paste("Brooklyn Defensive eFG% when opponent on a back-to-back: ", bkn_def_efg_b2b

# Part 2 Trends and Visualizations

# Question 6
# Identify at least 2 trends in scheduling over time

# Average change in longitude/latitude from one game to the next (how is it different from 2014-2018 in comparison to 2019-2023?)

# Home games for each team only
# merges locations so that timezone, latitude & longitude of game is displayed
home_games <- merge (schedule_df[schedule_df$home == 1, ], locations, by = "team")
home_games <- home_games %>%
  arrange(team,season,gamedate)

# Away games for each team only
away_games <- merge(schedule_df[schedule_df$home == 0, ], locations, by.x = "opponent", by.y = "team")
away_games <- away_games %>%
  arrange(team,season,gamedate)

# All games merged
merged_games <- rbind(home_games, away_games)
merged_games <- merged_games %>%
  arrange(team,season,gamedate)

# -----

# Calculate change in latitude / longitude between games for all games/seasons
merged_games <- merged_games %>%
  group_by(team, season) %>%
  mutate(prev_latitude = lag(latitude, default = first(latitude)), # latitude of game before
         prev_longitude = lag(longitude, default = first(longitude))) %>%   # longitude of game before
  mutate(lat_change = abs(latitude - prev_latitude),
         lon_change = abs(longitude - prev_longitude)) %>%
  ungroup()

#  Older games: games from 2014-2018 to represent the first half of years in the dataset
older_games <- merged_games %>%
  filter(season >= 2014 & season <= 2018)

# Recent games: games from 2019-2023 to represent the second half of years in the dataset
recent_games <- merged_games %>%
  filter(season >= 2019 & season <= 2023)

# ---

# Change in latitude (in consecutive games) per season for older games
avg_lat_change_per_season_older <- older_games %>%
  group_by(season) %>%
  summarize(avg_lat_change = mean(lat_change, na.rm = TRUE))

# Change in latitude (in consecutive games) per season for recent games
avg_lat_change_per_season_recent <- recent_games %>%
  group_by(season) %>%
  summarize(avg_lat_change = mean(lat_change, na.rm = TRUE))

# Combine
combined_avg_lat_change <- rbind(avg_lat_change_per_season_older, avg_lat_change_per_season_recent)

# Plot the average latitude change per season for both periods on the same graph
ggplot(combined_avg_lat_change, aes(x = season, y = avg_lat_change)) +
  geom_line(color = "blue") +
  geom_point(color = "orange") +
  labs(title = "Average Latitude Change Between Consecutive Games each Season",
       x = "Season",
       y = "Average Latitude Change")

paste("Average Change in Latitude Between Consecutive Games each Season")
paste(combined_avg_lat_change$season, ":", combined_avg_lat_change$avg_lat_change)

# Change in longitude (in consecutive games) per season for older games
avg_lon_change_per_season_older <- older_games %>%
  group_by(season) %>%
  summarize(avg_lon_change = mean(lon_change, na.rm = TRUE))

# Change in longitude (in consecutive games) per season for recent games
avg_lon_change_per_season_recent <- recent_games %>%
  group_by(season) %>%
  summarize(avg_lon_change = mean(lon_change, na.rm = TRUE))

# Combine
combined_avg_lon_change <- rbind(avg_lon_change_per_season_older, avg_lon_change_per_season_recent)

# Plot the average latitude change per season for both periods on the same graph
ggplot(combined_avg_lon_change, aes(x = season, y = avg_lon_change)) +
  geom_line(color = "blue") +
  geom_point(color = "orange") +
  labs(title = "Average Longitude Change Between Consecutive Games each Season",
       x = "Season",
       y = "Average Longitude Change")

paste("Average Change in Longitude Between Consecutive Games each Season")
paste(combined_avg_lon_change$season, ":", combined_avg_lon_change$avg_lon_change)

# 2nd Trend: Number of days since previous game

# Compare number of days since previous games

merged_games <- merged_games %>%
  group_by(team, season) %>%
  mutate(days_between_games = difftime(gamedate, lag(gamedate, default = first(gamedate)), units = "days")) %>% # days in between games
  ungroup()

# Filter
older_games <- merged_games %>%
  filter(season >= 2014 & season <= 2018)

recent_games <- merged_games %>%
  filter(season >= 2019 & season <= 2023)

# ---

# Calculate the average days between games per season for older games
avg_days_between_per_season_older <- older_games %>%
  group_by(season) %>%
  summarize(avg_days = mean(as.numeric(days_between_games), na.rm = TRUE))

# Calculate the average days between games per season for recent games
avg_days_between_per_season_recent <- recent_games %>%
  group_by(season) %>%
  summarize(avg_days = mean(as.numeric(days_between_games), na.rm = TRUE))

# Combine the dataframes
combined_avg_days_between <- rbind(avg_days_between_per_season_older, avg_days_between_per_season_recent)

# Plot: average days between games over each season
ggplot(combined_avg_days_between, aes(x = season, y = avg_days)) +
  geom_line(color = "blue") +
  geom_point(color ="orange") +
  labs(title = "Average Change in Days Since Last Game each Season",
       x = "Season",
       y = "Average Days Between Games")

paste("Average Change in Days Since Last Game each Season")
paste(combined_avg_days_between$season, ":", combined_avg_days_between$avg_days)

# Question 7
# Design plot tool to visualize a team's schedule

# Tool plots a team's schedule, showing their home/away games, as well as if the game is a back to back. 

schedule_tool <- function(schedule, team_str, year) { 
  # schedule: use one of the schedule dataframes
  # team_str: str that specifies which team
  # year: int that specifies which year
  
  # Filter from the given schedule dataframe to specify team & year
  team_schedule <- schedule %>%
    filter(team == team_str, season == year) %>%
    arrange(gamedate)   # Chronological order

  team_schedule <- team_schedule %>%
    
    # New variables to determine if the game is second half of back to back
    mutate(days_between_games = as.numeric(difftime(gamedate, lag(gamedate, default = first(gamedate)), units = "days"))) %>% 
    mutate(b2b = days_between_games == 1) %>%
    
    # New variable to determine if game is home or away
    mutate(home_or_away = ifelse(home == 1, "Home", "Away")) # 


  # Plot
  plot <- plot_ly(team_schedule, x = ~gamedate, y = 1,
               type = 'scatter', mode = 'markers',
               color = ~home_or_away, # Color by Home/Away
               colors = c("Away" = "orange", "Home" = "blue"), # Orange mark for away games, blue mark for home games
               
              marker = list(size = 10,
                            symbol = 'x'), 
               
               # Details of game: date, opponent, home/away, back to back or not
               text = ~paste(format(gamedate, '%Y-%m-%d'), '\n',
                             team_str, 'vs.', opponent, '(',home_or_away,')', '\n',
                             'Second Half of Back to Back:', b2b),
               hoverinfo = 'text') %>%

    # Graph titles
    layout(title = paste(team_str, year, "Schedule"),
           xaxis = list(title = "Date", showticklabels = TRUE),
           yaxis = list(title = "", showticklabels = FALSE))

  return(plot)
}

# Plot OKC and DEN 2024 schedules
schedule_tool(draft_schedule, "OKC", 2024)
schedule_tool(draft_schedule, "DEN", 2024)

# Question 8 on markdown file

# Question 9
# Estimate how many more/fewer regular season wins each team has had due to schedule-related factors from 2019-20 though 2023-24.

# Use recent_games (2019-2023) from previous cells

# Setup: Every team's year-by-year stats
team_season_by_season <- recent_games %>%
  group_by(team, season)%>%
  arrange(gamedate) %>%
  mutate(days_since_prev_game = difftime(gamedate, lag(gamedate, default = first(gamedate)), units = "days")) %>%
  mutate(b2b = days_since_prev_game == 1) %>%
  summarise(wins = sum(win),
            total_games = n(),
            win_pct = wins / total_games,   # Win percentage to determine opponent strength
            avg_days_since_lastgame = mean(days_between_games), # Days since last game
            avg_lat_change = mean (lat_change), # Average change in latitude between games
            avg_lon_change = mean (lon_change), # Average change in longitude between games
            num_b2bs = sum(b2b))    # Number of back to backs

team_season_by_season <- as.data.frame(team_season_by_season)

# Opponent strength: opponent win percentages
avg_opponent_win_pct <- team_season_by_season %>%
  group_by (team, season) %>%
  select(team, season, win_pct) %>%
  rename(opponent_team = team, opponent_win_pct = win_pct)

# Modified recent_games frame, now including opponent win %
recent_games_with_opponent_win_pct <- recent_games %>%
  left_join(avg_opponent_win_pct, by = c("season", "opponent" = "opponent_team"))

# Average opponent win percentage for each team each season
opponent_win_pct_by_season <- recent_games_with_opponent_win_pct %>%
  group_by(team, season) %>%
  summarise(avg_opponent_win_pct = mean(opponent_win_pct, na.rm = TRUE)) %>%
  ungroup()

# Merge to season by season dataframe
team_season_by_season <- team_season_by_season %>%
  left_join(opponent_win_pct_by_season, by = c("team", "season"))

# Find linear relationship between win percentage and schedule factors to estimate added wins/losses

# Estimates: take season by season df but filter out total games
estimates <- team_season_by_season %>%
  select(team, season, total_games, win_pct, avg_days_since_lastgame, avg_lat_change, avg_lon_change, num_b2bs, avg_opponent_win_pct) %>%
  mutate(avg_days_since_lastgame = as.numeric(avg_days_since_lastgame, units = "days")) # Converts to numeric specifically for model

# Model: estimates but only includes win percentage + schedule factors
model <- estimates %>%
    select(win_pct, avg_days_since_lastgame, avg_lat_change, avg_lon_change, num_b2bs, avg_opponent_win_pct)

# Win-loss model: linear model relating win_pct with combination of all included schedule factors
# Days since last game, latitude/longitude, number of back to backs, opponent win percentage (takes into account strength of schedule)
win_loss_model <- lm(win_pct ~ avg_days_since_lastgame + avg_lat_change + avg_lon_change + num_b2bs + avg_opponent_win_pct, data = model)

# Linear model prediction
estimates$predicted_win_pct <- predict(win_loss_model, newdata = estimates)

# Actual wins
# Prediction model used as expected wins per year
# Added wins is actual vs. predicted wins
estimates <- estimates %>%
  mutate(actual_wins = win_pct * total_games, # same as wins column in other dataframes
         expected_wins = predicted_win_pct * total_games,
         added_wins = actual_wins - expected_wins)

# Estimates how many wins or losses are added to each team
estimated_win_loss <- estimates %>%
  group_by(team) %>%
  summarise(actual_wins = sum(actual_wins, na.rm = TRUE),
            expected_wins = sum(expected_wins, na.rm = TRUE),
            added_wins = sum(added_wins, na.rm = TRUE))

# Team with most added wins
estimated_win_loss[which.max(estimated_win_loss$added_wins),]
# Team with least added wins
estimated_win_loss[which.min(estimated_win_loss$added_wins),]

# Model Diagnostic / Summary
summary(win_loss_model)
