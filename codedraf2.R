library(readr)
library(dplyr)
library(ggplot2)
data <- read_csv("output_file.csv")

vars <- c("date", "hometeam", "awayteam", "b365h", "b365a", "ftr", "temp_outlier", "dew_outlier", "humidity_outlier", "precip_outlier", "windgust_outlier", "windspeed_outlier", "solarradiation_outlier")
weather_vars = c("temp_outlier", "dew_outlier", "humidity_outlier", "precip_outlier", "windgust_outlier", "windspeed_outlier", "solarradiation_outlier")

subdata <- data %>% select(vars)
subdata <- subdata %>%
  mutate(
    team_winner = ifelse(ftr == 'H', hometeam, awayteam)
  )


for (col in weather_vars) {
  subdata[[col]] <- subdata[[col]] > 0
}

df <- subdata

team_colors <- c()

teams <- unique(df$awayteam)

result <- lapply(teams, function(x) {
  df[df$hometeam == x | df$awayteam == x, ]
})

# Name the list for easier identification
names(result) <- teams

teams

# Print the result
result

opponents <- result

# Iterate over each data frame in the list
for (i in seq_along(opponent)) {
  # Add a new column "opponent" by checking each row
  opponents[[i]]$opponent <- ifelse(
    opponents[[i]]$hometeam == teams[[i]],  # Check if the hometeam matches the list index
    opponents[[i]]$awayteam,       # Assign awayteam if TRUE
    opponents[[i]]$hometeam        # Assign hometeam if FALSE
  )
}

for (i in seq_along(opponent)) {
  # Add a new column "opponent" by checking each row
  if (opponents[[i]]$hometeam == teams[[i]]) {
    opponents[[i]]$team_odds <- opponents[[i]]$b365h
  }
  else {
    opponents[[i]]$opponent_odds <- opponents[[i]]$b365a
  }
}

for (i in seq_along(opponents)) {
  # Initialize team_odds and opponent_odds with NA
  opponents[[i]]$team_odds <- NA
  opponents[[i]]$opponent_odds <- NA
}

for (i in seq_along(opponents)) {
  # Initialize team_odds and opponent_odds with 0
  opponents[[i]]$team_odds <- 0
  opponents[[i]]$opponent_odds <- 0
}

for (i in seq_along(opponents)) {
  # Initialize team_odds and opponent_odds with NA
  opponents[[i]]$team_odds <- NA
  opponents[[i]]$opponent_odds <- NA
  
  # Assign values to both columns based on the condition
  opponents[[i]]$team_odds <- ifelse(
    opponents[[i]]$hometeam == teams[[i]],  # If the hometeam matches the team
    opponents[[i]]$b365h,                  # Assign b365h to team_odds
    opponents[[i]]$b365a                   # Otherwise, assign b365a to team_odds
  )
  
  opponents[[i]]$opponent_odds <- ifelse(
    opponents[[i]]$hometeam == teams[[i]],  # If the hometeam matches the team
    opponents[[i]]$b365a,                  # Assign b365a to opponent_odds
    opponents[[i]]$b365h                   # Otherwise, assign b365h to opponent_odds
  )
}

opponents

ggplot(opponents[[1]], aes(x = date)) +
  # Vertical lines for matches
  geom_segment(aes(
    xend = date,
    y = team_odds,
    yend = opponent_odds,
    color = team_winner
  ), size = 1.5) +
  # Points for team and opponent scores
  geom_point(aes(y = team_odds, color = "Team"), size = 4) +
  geom_point(aes(y = opponent_odds, color = "Opponent"), size = 4) +
  # Highlight extreme weather days
  geom_rect(
    data = filter(opponent[[1]], temp_outlier),
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

# Ensure date is in Date format
opponents[[1]]$date <- as.Date(opponents[[1]]$date)

# Horizontal plot
ggplot(opponents[[1]], aes(y = date)) +  # Flip x -> y
  # Horizontal lines for matches
  geom_segment(aes(
    yend = date,
    x = team_odds,
    xend = opponent_odds,
    color = team_winner
  ), size = 1.5) +
  # Points for team and opponent scores
  geom_point(aes(x = team_odds, color = "Team"), size = 4) +
  geom_point(aes(x = opponent_odds, color = "Opponent"), size = 4) +
  # Highlight extreme weather days
  geom_rect(
    data = filter(opponents[[1]], temp_outlier),
    aes(
      ymin = date - 0.5,
      ymax = date + 0.5,
      xmin = -Inf,
      xmax = Inf
    ),
    fill = "gray", alpha = 0.2, inherit.aes = FALSE
  ) +
  # Define custom colors
  scale_color_manual(
    values = c("Win" = "green", "Lose" = "red", "Team" = "blue", "Opponent" = "yellow")
  ) +
  # Add labels and theme
  labs(
    title = "Match Outcomes with Extreme Weather Highlights",
    y = "Date",
    x = "Odds",
    color = "Legend"
  ) +
  theme_minimal()

# Ensure date is in Date format
opponents[[i]]$date <- as.Date(opponents[[1]]$date)

# Plot
ggplot(opponents[[i]], aes(x = date)) +
  # Vertical lines for matches
  geom_segment(aes(
    xend = date,
    y = team_odds,
    yend = opponent_odds,
    color = team_winner
  ), size = 1.5) +
  # Points for team and opponent scores
  geom_point(aes(y = team_odds, color = "Team"), size = 4) +
  geom_point(aes(y = opponent_odds, color = "Opponent"), size = 4) +
  # Highlight extreme weather days
  geom_rect(
    data = filter(opponents[[i]], temp_outlier),
    aes(
      xmin = date - 0.5,
      xmax = date + 0.5,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "gray", alpha = 0.2, inherit.aes = FALSE
  ) +
  # Define custom colors
  scale_color_manual(
    values = c("Win" = "green", "Lose" = "red", "Team" = "blue", "Opponent" = "yellow")
  ) +
  # Add labels and theme
  labs(
    title = "Match Outcomes with Extreme Weather Highlights",
    x = "Date",
    y = "Odds",
    color = "Legend"
  ) +
  theme_minimal()

library(ggplot2)
library(dplyr)

# Define a function to generate the plot for opponents[[i]]
plot <- function(i, opponents) {
  # Ensure the date column is in Date format
  opponents[[i]]$date <- as.Date(opponents[[i]]$date)
  
  # Create the plot
  ggplot(opponents[[i]], aes(x = date)) +
    # Vertical lines for matches
    geom_segment(aes(
      xend = date,
      y = team_odds,
      yend = opponent_odds,
      color = team_winner
    ), size = 1.5) +
    # Points for team and opponent scores
    geom_point(aes(y = team_odds, color = "Team"), size = 4) +
    geom_point(aes(y = opponent_odds, color = "Opponent"), size = 4) +
    # Highlight extreme weather days
    geom_rect(
      data = filter(opponents[[i]], temp_outlier),
      aes(
        xmin = date - 0.5,
        xmax = date + 0.5,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "red", alpha = 0.2, inherit.aes = FALSE
    ) +
    # Define custom colors
    scale_color_manual(
      values = c("Win" = "green", "Lose" = "red", "Team" = "blue", "Opponent" = "yellow")
    ) +
    # Add labels and theme
    labs(
      title = paste("Match Outcomes for Opponent", i, "with Extreme Weather Highlights"),
      x = "Date",
      y = "Odds",
      color = "Legend"
    ) +
    theme_minimal()
}

# Example usage:
# Assuming `opponents` is a list of data frames
# plot_opponent(1, opponents)

plot(1, opponents)

library(ggplot2)
library(dplyr)


# Define a function to generate the plot for opponents[[i]]
plot_opponent <- function(i, opponents, temp_outlier_col = "temp_outlier") {
  weather_colors <- c("temp_outlier" = "red", "dew_outlier" = "lightblue", "humidity_outlier" = "gray", "precip_outlier" = "darkblue", "windgust_outlier" = "green", "windspeed_outlier" = "black", "solarradiation_outlier" = "orange")
  # Ensure the date column is in Date format
  opponents[[i]]$date <- as.Date(opponents[[i]]$date)
  fill_color <- weather_colors[[temp_outlier_col]]
  seg_color <- ifelse (
    opponents[[i]]$team_winner == teams[[i]],
    "green",
    "red"
  )
  # Create the plot
  ggplot(opponents[[i]], aes(x = date)) +
    # Vertical lines for matches
    geom_segment(aes(
      xend = date,
      y = team_odds,
      yend = opponent_odds
    ), size = 1.5,
    color = seg_color) +
    # Points for team and opponent scores
    geom_point(aes(y = team_odds, color = "Team"), size = 4) +
    geom_point(aes(y = opponent_odds, color = "Opponent"), size = 4) +
    # Highlight extreme weather days or other conditions
    geom_rect(
      data = filter(opponents[[i]], !!rlang::sym(temp_outlier_col)),
      aes(
        xmin = date - 0.5,
        xmax = date + 0.5,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = fill_color, alpha = 0.2, inherit.aes = FALSE
    ) +
    # Define custom colors
    scale_color_manual(
      values = c("Team" = "blue", "Opponent" = "yellow")
    ) +
    # Add labels and theme
    labs(
      title = paste("Match Outcomes for Opponent", i, "with Highlighted Conditions"),
      x = "Date",
      y = "Odds",
      color = "Legend"
    ) +
    theme_minimal()
}

# Example usage:
# Assuming `opponents` is a list of data frames
 #plot_opponent(1, opponents, temp_outlier_col = "precip_outlier")
plot_opponent(2, opponents, temp_outlier_col = "temp_outlier")


