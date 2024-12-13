library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)

# Read and prepare the data
soccer_data <- read.csv("data/soccer_main.csv")

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

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Weather & Soccer Outcomes Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
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
      )
    )
  )
)

# Server
server <- function(input, output) {
  
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