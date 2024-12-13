# Load necessary libraries
library(shiny)
library(shinythemes)
library(DT)
library(pacman)
pacman::p_load(readr, ggplot2, dplyr, shiny, rlang, here, ggimage, curl, rsvg, stringr)

# Load the dataset
soccer_data <- read_csv(here("data", "soccer_main.csv"))

# Data Preprocessing
selected_columns <- soccer_data %>% select("temp", "dew", "humidity", "precip", "windgust", "windspeed", "solarradiation")
columns <- c("temp", "dew", "humidity", "precip", "windgust", "windspeed", "solarradiation")

for (col in columns) {
  Q1 <- quantile(selected_columns[[col]], 0.2, na.rm = TRUE)
  Q3 <- quantile(selected_columns[[col]], 0.8, na.rm = TRUE)
  
  dummy_col_name <- paste0(col, " extreme")
  soccer_data[[dummy_col_name]] <- ifelse(selected_columns[[col]] < Q1 | selected_columns[[col]] > Q3, 1, 0)
}

vars <- c(
  "date", "hometeam", "awayteam", "b365h", "b365a", "ftr", 
  "temp extreme", "dew extreme", "humidity extreme", 
  "precip extreme", "windgust extreme", "windspeed extreme", "solarradiation extreme"
)
weather_vars <- c(
  "temp extreme", "dew extreme", "humidity extreme", 
  "precip extreme", "windgust extreme", "windspeed extreme", "solarradiation extreme"
)

subdata <- soccer_data %>% select(vars) %>%
  mutate(team_winner = ifelse(ftr == 'H', hometeam, awayteam))

for (col in weather_vars) {
  subdata[[col]] <- subdata[[col]] > 0
}

df <- subdata
teams <- unique(df$awayteam)

result <- lapply(teams, function(x) {
  df[df$hometeam == x | df$awayteam == x, ]
})

names(result) <- teams
opponents <- result

for (i in seq_along(opponents)) {
  opponents[[i]]$team_odds <- NA
  opponents[[i]]$opponent_odds <- NA
  opponents[[i]]$opponent <- NA
  
  opponents[[i]]$opponent <- ifelse(
    opponents[[i]]$hometeam == teams[[i]],
    opponents[[i]]$awayteam,
    opponents[[i]]$hometeam
  )
  
  opponents[[i]]$team_odds <- ifelse(
    opponents[[i]]$hometeam == teams[[i]],  
    opponents[[i]]$b365h,                  
    opponents[[i]]$b365a                   
  )
  
  opponents[[i]]$opponent_odds <- ifelse(
    opponents[[i]]$hometeam == teams[[i]],  
    opponents[[i]]$b365a,                  
    opponents[[i]]$b365h                   
  )
}

# Define a mapping of team names to their logo URLs
logo_urls <- c(
  "Arsenal" = "https://upload.wikimedia.org/wikipedia/en/5/53/Arsenal_FC.svg",
  "Chelsea" = "https://upload.wikimedia.org/wikipedia/en/c/cc/Chelsea_FC.svg",
  "Tottenham" = "https://upload.wikimedia.org/wikipedia/en/b/b4/Tottenham_Hotspur.svg",
  "Leeds" = "https://upload.wikimedia.org/wikipedia/en/5/54/Leeds_United_F.C._logo.svg",
  "Brighton" = "https://upload.wikimedia.org/wikipedia/en/d/d0/Brighton_and_Hove_Albion_FC_crest.svg",
  "Crystal Palace" = "https://upload.wikimedia.org/wikipedia/en/a/a2/Crystal_Palace_FC_logo_%282022%29.svg",
  "Southampton" = "https://upload.wikimedia.org/wikipedia/en/c/c9/FC_Southampton.svg",
  "Wolves" = "https://upload.wikimedia.org/wikipedia/en/c/c9/Wolverhampton_Wanderers_FC_crest.svg",
  "Aston Villa" = "https://upload.wikimedia.org/wikipedia/en/9/9a/Aston_Villa_FC_new_crest.svg",
  "Liverpool" = "https://upload.wikimedia.org/wikipedia/en/0/0c/Liverpool_FC.svg",
  "West Ham" = "https://upload.wikimedia.org/wikipedia/en/c/c2/West_Ham_United_FC_logo.svg",
  "Man City" = "https://upload.wikimedia.org/wikipedia/en/e/eb/Manchester_City_FC_badge.svg",
  "Burnley" = "https://upload.wikimedia.org/wikipedia/en/6/6d/Burnley_FC_Logo.svg",
  "Newcastle" = "https://upload.wikimedia.org/wikipedia/en/5/56/Newcastle_United_Logo.svg",
  "Brentford" = "https://upload.wikimedia.org/wikipedia/en/2/2a/Brentford_FC_crest.svg",
  "Everton" = "https://upload.wikimedia.org/wikipedia/en/7/7c/Everton_FC_logo.svg",
  "Norwich" = "https://upload.wikimedia.org/wikipedia/en/1/17/Norwich_City_FC_logo.svg",
  "Watford" = "https://upload.wikimedia.org/wikipedia/en/e/e2/Watford.svg",
  "Man United" = "https://upload.wikimedia.org/wikipedia/en/7/7a/Manchester_United_FC_crest.svg",
  "Leicester" = "https://upload.wikimedia.org/wikipedia/en/2/2d/Leicester_City_crest.svg",
  "Nottingham" = "https://upload.wikimedia.org/wikipedia/en/e/e5/Nottingham_Forest_F.C._logo.svg",
  "Bournemouth" = "https://upload.wikimedia.org/wikipedia/en/e/e5/AFC_Bournemouth_%282013%29.svg",
  "Fulham" = "https://upload.wikimedia.org/wikipedia/en/e/eb/Fulham_FC_%28shield%29.svg",
  "Default" = "https://upload.wikimedia.org/wikipedia/commons/a/ac/Tottenham_Hotspur.svg"
)

# Validate and replace invalid URLs with a default placeholder
validate_logo_urls <- function(logo_urls) {
  sapply(logo_urls, function(url) {
    tryCatch({
      if (curl::curl_fetch_memory(url)$status_code == 200) {
        return(url)
      } else {
        return(logo_urls["Default"])
      }
    }, error = function(e) {
      return(logo_urls["Default"])
    })
  })
}
logo_urls <- validate_logo_urls(logo_urls)


# Load the dataset
df <- read_csv(here("data","soccer_main.csv"))

# Data Processing (yash)
grouped_df <- df %>%
  group_by(referee) %>%
  summarize(
    hy = sum(hy),
    ay = sum(ay),
    hr = sum(hr),
    ar = sum(ar),
    hf = sum(hf),
    af = sum(af)
  )

grouped_df_used <- grouped_df %>%
  mutate(referee = str_replace_all(referee, " ", "_"))

# Data PROCESSING (PRAJWAL)
# Convert temperature to numeric and create temperature bins
soccer_data$temp <- as.numeric(soccer_data$temp)
soccer_data$temp_category <- cut(soccer_data$temp, 
                                 breaks = c(-Inf, 5, 10, 15, 20, Inf),
                                 labels = c("Very Cold (<5°C)", "Cold (5-10°C)", 
                                            "Mild (10-15°C)", "Warm (15-20°C)", 
                                            "Hot (>20°C)"))

# Calculate draw probabilities for each temperature point
draw_prob_by_temp <- soccer_data %>%
  group_by(temp) %>%
  summarise(
    matches = n(),
    draws = sum(ftr == "D"),
    draw_prob = draws / matches,
    avg_prematch_odds = mean(b365d, na.rm = TRUE)  # only use pre-match odds
  ) %>%
  filter(matches >= 5)  # Filter out temperatures with too few matches



#PLOTS----------------------


plot_opponent <- function(team_name, opponents, teams, temp_outlier_col, logo_urls) {
  weather_colors <- c(
    "temp extreme" = "red", "dew extreme" = "lightblue",
    "humidity extreme" = "gray", "precip extreme" = "darkblue",
    "windgust extreme" = "green", "windspeed extreme" = "black",
    "solarradiation extreme" = "orange"
  )
  
  if (!team_name %in% names(opponents)) {
    stop(paste("Team", team_name, "not found in the opponents list."))
  }
  
  data <- opponents[[team_name]]
  data <- data %>%
    filter(!is.na(team_odds), !is.na(opponent_odds))  # Exclude rows with missing odds
  
  data$date <- as.Date(data$date)
  
  if (!temp_outlier_col %in% names(data)) {
    stop(paste("Column", temp_outlier_col, "does not exist in the dataset for team", team_name, "."))
  }
  
  fill_color <- weather_colors[[temp_outlier_col]]
  seg_color <- ifelse(data$team_winner == team_name, "green", "red")
  
  # Add team logo URLs to the data
  data$team_logo <- logo_urls[data$hometeam]
  data$opponent_logo <- logo_urls[data$awayteam]
  
  ggplot(data, aes(x = date)) +
    geom_segment(aes(
      xend = date,
      y = team_odds,
      yend = opponent_odds
    ), size = 1.5, color = seg_color) +
    geom_image(aes(
      y = team_odds,
      image = team_logo
    ), size = 0.04) +  # Adjust size as necessary
    geom_image(aes(
      y = opponent_odds,
      image = opponent_logo
    ), size = 0.04) +  # Adjust size as necessary
    geom_rect(
      data = filter(data, !!rlang::sym(temp_outlier_col)),
      aes(
        xmin = date - 2,
        xmax = date + 2,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = fill_color, alpha = 0.2, inherit.aes = FALSE
    ) +
    labs(
      title = paste("Match Outcomes for", team_name),
      x = "Date",
      y = "Odds"
    ) +
    theme_minimal() +
    theme(
      aspect.ratio = 0.4,
      plot.title = element_text(hjust = 0.5, size = 20)  # Center align the title
    )
}

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Premier League and Weather Analysis Dashboard"),
  
  navbarPage(
    "Soccer Insights",
    
    tabPanel(
      "Extreme Weather",
      fluidRow(
        column(
          6,  # Adjust the width for the dropdowns
          selectInput("team_name", "Select Team:", choices = NULL)
        ),
        column(
          6,
          selectInput("temp_outlier_col", "Select extreme weather condition:", choices = c(
            "temp extreme", "dew extreme", "humidity extreme", 
            "precip extreme", "windgust extreme", "windspeed extreme", 
            "solarradiation extreme"
          ))
        )
      ),
      fluidRow(
        column(
          12,  # Full width for the plot
          plotOutput("opponentPlot", height = "600px")  # Set a larger height for better display
        )
      )
    ),
    
    tabPanel(
      "Referee Performance",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "referee",
            label = "Select a Referee:",
            choices = unique(grouped_df$referee)
          )
        ),
        mainPanel(
          plotOutput("pieChart")
        )
      )
    ),
    tabPanel(
      "Prajwal",
      fluidRow(
        box(width = 12,
            title = "Cold Weather Impact on Draw Probability",
            status = "primary",
            plotOutput("cold_impact_plot", height = "400px")
        )
      ),
      fluidRow(
        box(width = 6,
            title = "Draw Probability vs Temperature",
            status = "info",
            plotOutput("draw_prob_plot")
        ),
        box(width = 6,
            title = "Betting Odds Evolution (Pre-match vs Live)",
            status = "warning",
            plotOutput("odds_evolution_plot")
        )
      )
    ),
    tabPanel(
      "Raw",
      fluidRow(
        column(12,
               DTOutput("raw_data_table")
        )
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "team_name", choices = names(opponents))
  })
  
  output$opponentPlot <- renderPlot({
    req(input$team_name, input$temp_outlier_col)
    plot_opponent(
      team_name = input$team_name,
      opponents = opponents,
      teams = teams,
      temp_outlier_col = input$temp_outlier_col,
      logo_urls = logo_urls
    )
  })
  
  output$raw_data_table <- renderDT({
    datatable(
      soccer_data,
      options = list(pageLength = 10, searchHighlight = TRUE, autoWidth = TRUE, scrollX = TRUE),
      filter = "top"
    )
  })
  
  # Function to plot referee stats
  plot_referee_stats <- function(referee_name) {
    referee_name <- gsub(" ", "_", referee_name)
    
    # Filter data for the specific referee
    referee_data <- grouped_df_used %>%
      filter(referee == referee_name)
    
    # Calculate percentages for home and away teams
    home_stats <- data.frame(
      category = c("Cards", "Fouls"),
      proportion = c(
        (referee_data$hr + referee_data$hy) / referee_data$hf * 100,
        100 - (referee_data$hr + referee_data$hy) / referee_data$hf * 100
      )
    )
    away_stats <- data.frame(
      category = c("Cards", "Fouls"),
      proportion = c(
        (referee_data$ar + referee_data$ay) / referee_data$af * 100,
        100 - (referee_data$ar + referee_data$ay) / referee_data$af * 100
      )
    )
    
    # Add positions for text labels within the segments
    home_stats$ypos <- cumsum(home_stats$proportion) - home_stats$proportion / 2
    away_stats$ypos <- cumsum(away_stats$proportion) - away_stats$proportion / 2
    
    # Plot for Home Team
    p1 <- ggplot(home_stats, aes(x = "", y = proportion, fill = category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = paste0("Home Team: ")) +
      theme_void() +
      scale_fill_manual(values = c("#F05039", "#1F449C")) +
      geom_text(
        aes(y = ypos, label = paste0(round(proportion, 2), "%")),
        size = 4, color = "black"
      )
    
    # Plot for Away Team
    p2 <- ggplot(away_stats, aes(x = "", y = proportion, fill = category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = paste0("Away Team: ")) +
      theme_void() +
      scale_fill_manual(values = c("#C99738", "#3D65A5")) +
      geom_text(
        aes(y = ypos, label = paste0(round(proportion, 2), "%")),
        size = 4, color = "black"
      )
    
    # Combine and return plots
    grid.arrange(p1, p2, ncol = 2, top = paste0("Card and Foul Distribution for Referee: ", referee_name))
  }  
  # Render the pie chart
  output$pieChart <- renderPlot({
    req(input$referee)  # Ensure referee input is not null
    plot_referee_stats(input$referee)
  })
  # New cold weather impact visualization
  output$cold_impact_plot <- renderPlot({
    tryCatch({
      # Create binned temperature data
      binned_data <- soccer_data %>%
        mutate(temp_bin = cut(temp, breaks = seq(min(temp, na.rm = TRUE), 
                                                 max(temp, na.rm = TRUE), 
                                                 length.out = 20))) %>%
        group_by(temp_bin) %>%
        summarise(
          avg_temp = mean(temp, na.rm = TRUE),
          draw_prop = mean(ftr == "D", na.rm = TRUE),
          total_games = n()
        ) %>%
        filter(total_games >= 5)  # Filter bins with too few games
      
      # Create the plot
      ggplot() +
        # Add stacked bar chart for all outcomes
        geom_bar(data = soccer_data, 
                 aes(x = temp, fill = ftr), 
                 position = "fill", 
                 alpha = 0.5) +
        # Add draw probability line (zig-zag)
        geom_line(data = binned_data,
                  aes(x = avg_temp, y = draw_prop, color = "Observed Draw Rate"),
                  size = 1.5) +
        # Add confidence interval (smooth)
        geom_smooth(data = binned_data,
                    aes(x = avg_temp, y = draw_prop, color = "Smoothed Trend"),
                    method = "loess",
                    fill = "lightblue",
                    alpha = 0.3) +
        # Customize appearance
        scale_fill_manual(values = c("H" = "#3498db", 
                                     "D" = "#e74c3c", 
                                     "A" = "#2ecc71"),
                          labels = c("Home Win", "Draw", "Away Win")) +
        scale_color_manual(values = c("Observed Draw Rate" = "#e74c3c",
                                      "Smoothed Trend" = "#3498db")) +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(x = "Temperature (°C)", 
             y = "Proportion of Outcomes",
             title = "Match Outcomes and Draw Probability by Temperature",
             subtitle = "Red line shows observed draw rate, blue line shows smoothed trend",
             fill = "Match Result",
             color = "Draw Probability") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12, color = "darkgray"),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.margin = margin()
        )
    }, error = function(e) {
      print(paste("Error in cold_impact_plot:", e$message))
      return(NULL)
    })
  })
  
  # Draw probability plot
  output$draw_prob_plot <- renderPlot({
    tryCatch({
      ggplot(draw_prob_by_temp, aes(x = temp, y = draw_prob)) +
        geom_point(alpha = 0.5, color = "#e74c3c") +
        geom_smooth(method = "loess", color = "#c0392b", se = TRUE) +
        geom_hline(yintercept = mean(soccer_data$ftr == "D"), 
                   linetype = "dashed", color = "gray50") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(x = "Temperature (°C)", 
             y = "Draw Probability",
             title = "Draw Probability vs Temperature") +
        theme_minimal()
    }, error = function(e) {
      print(paste("Error in draw_prob_plot:", e$message))
      return(NULL)
    })
  })
  
  # Odds evolution plot
  output$odds_evolution_plot <- renderPlot({
    tryCatch({
      # Calculate average odds and standard errors for each temperature category
      odds_summary <- soccer_data %>%
        group_by(temp_category) %>%
        summarise(
          avg_odds = mean(b365d, na.rm = TRUE),
          se_odds = sd(b365d, na.rm = TRUE) / sqrt(n()),
          n_matches = n()
        ) %>%
        filter(!is.na(temp_category))
      
      # Create the plot
      ggplot(odds_summary, aes(x = temp_category, y = avg_odds)) +
        # Add error bars
        geom_errorbar(aes(ymin = avg_odds - se_odds, 
                          ymax = avg_odds + se_odds),
                      width = 0.2, color = "#e74c3c") +
        # Add points
        geom_point(size = 3, color = "#e74c3c") +
        # Add line connecting points
        geom_line(aes(group = 1), color = "#e74c3c") +
        # Customize appearance
        labs(x = "Temperature Category",
             y = "Average Draw Odds",
             title = "Draw Odds by Temperature",
             subtitle = paste("Based on", sum(odds_summary$n_matches), "matches")) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray90")
        )
    }, error = function(e) {
      print(paste("Error in odds_evolution_plot:", e$message))
      return(NULL)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)