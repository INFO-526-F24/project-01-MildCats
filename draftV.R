library(readr)
data <- read_csv("data/soccer_main.csv")
# dew, humidity, windgust, windspeed, visibility, 
correlation <- cor(data$b365h, data$uvindex, method = "pearson", use = "complete.obs")
print(correlation)

# 1. select weather variables, exclude solarradiatoin or solar energy because high correlation
# 2. temp, dew, humidity, windgust, windspeed, visibility, feelslike, precip, uvindex (radio button)
# 3. define extremeties for each variable
# teams (drop down)
# 4. define who is favorite or underdog for that match based on pregame odds
# 5. visualize how often a surprise ocurrs based on how often the underdog wins
# 6. team and weather wise, visualize matches played, highlight days of extreme weather, 
#     visualize odds for both chosen and opposite team for each date, and find a cool way to visualize the result. 
# 

library(dplyr)

# Step 2: Select specific columns
selected_columns <- data %>% select("temp", "dew", "humidity", "precip", "windgust", "windspeed", "solarradiation")
columns <- c("temp", "dew", "humidity", "precip", "windgust", "windspeed", "solarradiation")

# View the selected columns
print(selected_columns)

count_outliers <- function(column) {
  
  
  # Calculate quartiles and IQR
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  
  # Count values outside the bounds
  outliers <- column[column < Q1 | column > Q3]
  return(length(outliers))
}

# Step 4: Apply the function to each selected column
outlier_counts <- sapply(selected_columns, count_outliers)

# Step 5: Output the result
print(outlier_counts)


for (col in columns) {
  # Calculate Q1, Q3, and IQR
  Q1 <- quantile(selected_columns[[col]], 0.25, 'na.rm' = TRUE)
  Q3 <- quantile(selected_columns[[col]], 0.75, 'na.rm' = TRUE)
  
  # Create dummy column
  dummy_col_name <- paste0(col, "_outlier")
  data[[dummy_col_name]] <- ifelse(selected_columns[[col]] < Q1 | selected_columns[[col]] > Q3, 1, 0)
}

write.csv(data, "output_file.csv", row.names = FALSE)
