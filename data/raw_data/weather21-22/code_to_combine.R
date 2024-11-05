setwd("data/raw_data/weather21-22")
library(dplyr)
library(readr)
file_list <- list.files()
file_list
merged_data <- lapply(file_list, read.csv) %>% bind_rows()
write.csv(merged_data, "merged_weather-21-22.csv", row.names = FALSE)
