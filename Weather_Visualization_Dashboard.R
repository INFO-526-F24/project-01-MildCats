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
                                 breaks = c(-Inf, 10, 15, 20, Inf),
                                 labels = c("Cold (<10°C)", "Cool (10-15°C)", 
                                            "Mild (15-20°C)", "Warm (>20°C)"))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Weather & Soccer Outcomes Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Detailed Analysis", tabName = "detailed", icon = icon("chart-line")),
      menuItem("Betting Analysis", tabName = "betting", icon = icon("money-bill"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Temperature Distribution by Match Outcome",
                  status = "primary",
                  plotOutput("temp_outcome_plot")
                ),
                box(
                  title = "Draw Percentage by Temperature Category",
                  status = "info",
                  plotOutput("draw_temp_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Summary Statistics",
                  status = "success",
                  tableOutput("summary_stats")
                )
              )
      ),
      
      # Detailed Analysis Tab
      tabItem(tabName = "detailed",
              fluidRow(
                box(
                  title = "Temperature Range Filter",
                  sliderInput("temp_range", "Select Temperature Range (°C):",
                              min = min(soccer_data$temp, na.rm = TRUE),
                              max = max(soccer_data$temp, na.rm = TRUE),
                              value = c(0, 30))
                )
              ),
              fluidRow(
                box(
                  title = "Filtered Match Results",
                  DTOutput("filtered_results")
                )
              )
      ),
      
      # Betting Analysis Tab
      tabItem(tabName = "betting",
              fluidRow(
                box(
                  title = "Average Draw Odds by Temperature",
                  plotOutput("odds_temp_plot")
                ),
                box(
                  title = "Betting Odds Analysis",
                  tableOutput("odds_analysis")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Temperature vs Outcome Plot
  output$temp_outcome_plot <- renderPlot({
    ggplot(soccer_data, aes(x = temp, fill = ftr)) +
      geom_density(alpha = 0.5) +
      labs(x = "Temperature (°C)", y = "Density", fill = "Match Result") +
      theme_minimal()
  })
  
  # Draw Percentage by Temperature Plot
  output$draw_temp_plot <- renderPlot({
    soccer_data %>%
      group_by(temp_category) %>%
      summarise(
        draw_pct = mean(ftr == "D") * 100,
        n = n()
      ) %>%
      ggplot(aes(x = temp_category, y = draw_pct)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "Temperature Category", y = "Draw Percentage (%)") +
      theme_minimal()
  })
  
  # Summary Statistics
  output$summary_stats <- renderTable({
    soccer_data %>%
      group_by(temp_category) %>%
      summarise(
        total_matches = n(),
        draws = sum(ftr == "D"),
        draw_percentage = round(mean(ftr == "D") * 100, 2),
        avg_temp = round(mean(temp, na.rm = TRUE), 1)
      )
  })
  
  # Filtered Results
  output$filtered_results <- renderDT({
    soccer_data %>%
      filter(temp >= input$temp_range[1], temp <= input$temp_range[2]) %>%
      select(date, hometeam, awayteam, temp, ftr, b365d) %>%
      datatable()
  })
  
  # Odds vs Temperature Plot
  output$odds_temp_plot <- renderPlot({
    ggplot(soccer_data, aes(x = temp, y = b365d)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "loess") +
      labs(x = "Temperature (°C)", y = "Draw Odds (Bet365)") +
      theme_minimal()
  })
  
  # Betting Odds Analysis
  output$odds_analysis <- renderTable({
    soccer_data %>%
      group_by(temp_category) %>%
      summarise(
        avg_draw_odds = round(mean(b365d, na.rm = TRUE), 2),
        actual_draws = sum(ftr == "D"),
        total_matches = n(),
        implied_prob = round(1/mean(b365d, na.rm = TRUE) * 100, 2),
        actual_prob = round(mean(ftr == "D") * 100, 2)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
