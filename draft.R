library(readr)
data <- read_csv("data/soccer_main.csv")
# dew, humidity, windgust, windspeed, visibility, 
correlation <- cor(data$solarradiation, data$uvindex, method = "pearson", use = "complete.obs")
print(correlation)

# 1. select weather variables, exclude solarradiatoin or solar energy because high correlation
# 2. temp, dew, humidity, windgust, windspeed, visibility, feelslike, precip, uvindex

# 3. define extremeties for each variable
# 4. define who is favorite or underdog for that match based on pregame odds
# 5. visualize how often a surprise ocurrs based on how often the underdog wins
# 6. team and weather wise, visualize matches played, highlight days of extreme weather, 
#     visualize odds for both chosen and opposite team for each date, and find a cool way to visualize the result. 
# 

# Load libraries
library(ggplot2)
library(dplyr)

library(readr)
data <- read_csv("data/soccer_main.csv")

# Calculate magnitude and team colors
data <- data %>%
  mutate(
    magnitude = abs(b365h - b365a),
    team_winner = ifelse(htr == 'H', hometeam, awayteam)
  )

# Create the lollipop chart
ggplot(data, aes(x = date, y = 0)) +
  geom_segment(aes(
    xend = match_id,
    y = -magnitude / 2,
    yend = magnitude / 2,
    color = team_winner
  ), size = 1) +
  geom_point(aes(y = magnitude / 2, color = "Team 1"), size = 4) +
  geom_point(aes(y = -magnitude / 2, color = "Team 2"), size = 4) +
  scale_color_manual(values = c("Team 1" = "blue", "Team 2" = "red")) +
  labs(
    title = "Match Results Visualization",
    x = "Match ID",
    y = "Magnitude (Win/Loss Margin)"
  ) +
  theme_minimal()





# Load libraries
library(ggplot2)
library(dplyr)

# Sample data
data <- data.frame(
  date = as.Date(c("2024-12-01", "2024-12-02", "2024-12-03")),
  team = c("Team A", "Team B", "Team C"),
  opponent = c("Team B", "Team C", "Team A"),
  team_score = c(30, 20, 25),
  opponent_score = c(20, 25, 15),
  extreme_weather = c(TRUE, FALSE, TRUE)
)



# Add magnitude and result
data <- data %>%
  mutate(
    margin = abs(team_score - opponent_score),
    result = ifelse(team_score > opponent_score, "Win", "Lose")
  )

# Create the plot
ggplot(data, aes(x = date)) +
  # Vertical lines for matches
  geom_segment(aes(
    xend = date,
    y = team_score,
    yend = opponent_score,
    color = result
  ), size = 1.5) +
  # Points for team and opponent scores
  geom_point(aes(y = team_score, color = "Team"), size = 4) +
  geom_point(aes(y = opponent_score, color = "Opponent"), size = 4) +
  # Highlight extreme weather days
  geom_rect(
    data = filter(data, extreme_weather),
    aes(
      xmin = as.Date(date) - 0.5,
      xmax = as.Date(date) + 0.5,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "gray", alpha = 0.2, inherit.aes = FALSE
  ) +
  scale_color_manual(values = c("Win" = "green", "Lose" = "red", "Team" = "blue", "Opponent" = "yellow")) +
  labs(
    title = "Match Outcomes with Extreme Weather Highlights",
    x = "Date",
    y = "Score",
    color = "Legend"
  ) +
  theme_minimal()

