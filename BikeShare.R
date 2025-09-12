library(tidyverse)
library(tidymodels)
library(DataExplorer)
library(GGally)
library(skimr)
library(vroom)
library(patchwork)



data_csv <- vroom("train.csv")

average_per_weather <- data_csv |>
  group_by(weather) |>
  summarize(mean_rentals = mean(count, na.rm = TRUE))

print(average_per_weather)

weather_bar_plot <- ggplot(average_per_weather, aes(x = weather, y = mean_rentals)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Counts of Each Weather Type By Hour",
       x = "Category",
       y = "Count") +
  theme_minimal()

weather_bar_plot

temp_histogram <- ggplot(data_csv, aes(x = atemp)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Histogram of Adjusted Temperatures By Hour",
       x = "Temperature (°C)",
       y = "Frequency") +
  theme_minimal()

temp_histogram

correlations_filtered <- data_csv |>
  select(weather, temp, atemp, humidity, windspeed, count)

GGally::ggpairs(correlations_filtered)

correlations <- DataExplorer::plot_correlation(data = data_csv)

missing <- DataExplorer::plot_missing(data_csv)

hists <- DataExplorer::plot_histogram(data_csv)

glimpse <- dplyr::glimpse(data_csv)

(weather_bar_plot | temp_histogram)# / (plot1 + plot2)

(weather_bar_plot | correlations) / (missing + hists)

