pacman:: p_load(ggplot2,
                dsbox,
                dplyr,
                readr,
                ggridges,
                tidyverse,
                fs,
                janitor,
                lubridate,
                formatR,
                scales,
                ggforce,
                palmerpenguins,
                stringr,
                ggridges,
                gridExtra)

df <- read_csv("soccer_main.csv")

grouped_df <- df |>
  group_by(referee) |>
  summarize(
    hy = sum(hy),
    ay = sum(ay),
    hr = sum(hr),
    ar = sum(ar),
    hf = sum(hf),
    af = sum(af)
  )

grouped_df_used <- grouped_df |>
  mutate(referee = str_replace_all(referee, " ", "_"))





# Define UI
ui <- fluidPage(
  titlePanel("Referee Performance"),
  sidebarLayout(
    sidebarPanel(
      selectInput("referee", "Select a Referee:", 
                  choices = unique(grouped_df$referee))
    ),
    mainPanel(
      plotOutput("pieChart")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  plot_referee_stats <- function(referee_name, data) {
    print(referee_name)
    referee_name <- gsub(" ", "_", input$referee)
     
    # Filter data for the specific referee
    referee_data <- grouped_df_used |>
      filter(referee == referee_name)
    
    # Calculate total cards and fouls for home and away teams
    total_home_cards <- referee_data$hr + referee_data$hy
    total_home_fouls <- referee_data$hf
    # Calculate percentage of cards to fouls
    home_card_percentage <- (total_home_cards / total_home_fouls) * 100
    home_foul_percentage <- 100 - home_card_percentage
    
    total_away_cards <- referee_data$ar + referee_data$ay
    total_away_fouls <- referee_data$af
    away_card_percentage <- (total_away_cards / total_away_fouls) * 100
    away_foul_percentage <- 100 - away_card_percentage
    
    # Create data frames for plotting
    home_data <- data.frame(
      category = c("Cards", "Fouls"),
      proportion = c(home_card_percentage, home_foul_percentage)
    )
    
    away_data <- data.frame(
      category = c("Cards", "Fouls"),
      proportion = c(away_card_percentage, away_foul_percentage)
    )
    
    p1 <- ggplot(home_data, aes(x = "", y = proportion, fill = category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = paste0("Home Team: ", input$referee)) +
      theme_void() +
      scale_fill_manual(values = c("red", "yellow")) +
      geom_text(aes(x = 1, y = cumsum(proportion) - proportion/2, label = paste0(round(proportion,2), "%")), size = 4)
    
    p2 <- ggplot(away_data, aes(x = "", y = proportion, fill = category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = paste0("Away Team: ", input$referee)) +
      theme_void() +
      scale_fill_manual(values = c("red", "yellow")) +
      geom_text(aes(x = 1, y = cumsum(proportion) - proportion/2, label = paste0(round(proportion,2), "%")), size = 4)
    
    # Combine and display the plots
    grid.arrange(p1, p2, ncol = 2, top = paste0("Card and Foul Distribution for Referee: ", input$referee))
  }
  
  # Function to create the pie chart
  create_pie_chart <- function(stats) {
    # Create subplots for home and away teams
    print(home_data)

  }
  
  output$pieChart <- renderPlot({
    
    # Calculate fouls and cards
    stats <- plot_referee_stats(input$referee)
    
    # Create the pie chart
    create_pie_chart(stats)
  })
}

# Run the app
shinyApp(ui = ui, server = server)