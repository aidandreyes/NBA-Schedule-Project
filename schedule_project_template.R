# Setup and Data
library(plotly)
library(tidyverse)

schedule <- read_csv("schedule.csv")
draft_schedule <- read_csv("schedule_24_partial.csv")
locations <- read_csv("locations.csv")
game_data <- read_csv("team_game_data.csv")

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
paste("Brooklyn Defensive eFG% when opponent on a back-to-back: ", bkn_def_efg_b2b)



