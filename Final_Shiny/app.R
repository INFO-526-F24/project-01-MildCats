# Load necessary libraries
library(shiny)
library(shinythemes)
library(DT)
library(pacman)
pacman::p_load(readr, ggplot2, dplyr, shiny, rlang, here, ggimage, curl, rsvg)

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
    ), size = 0.03) +  # Adjust size as necessary
    geom_image(aes(
      y = opponent_odds,
      image = opponent_logo
    ), size = 0.03) +  # Adjust size as necessary
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
      plot.title = element_text(hjust = 0.5)  # Center align the title
    )
}

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Premier League and Weather Analysis Dashboard"),
  
  navbarPage(
    "Soccer Insights",
    
    tabPanel(
      "Vishnu",
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
    
    tabPanel("Raw", fluidPage(DTOutput("raw_data_table")))
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
}

# Run the application
shinyApp(ui = ui, server = server)