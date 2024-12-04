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