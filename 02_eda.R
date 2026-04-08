pkgs <- c("tidyverse", "janitor", "skimr", "scales")
installed <- rownames(installed.packages())
for (p in pkgs) if (!p %in% installed) install.packages(p, quiet = TRUE)

library(tidyverse)
library(janitor)
library(skimr)
library(scales)

dir.create("output", showWarnings = FALSE)
dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)

df <- read_csv("output/food_delivery_clean.csv", show_col_types = FALSE) |> clean_names()

missing_summary <- df |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "missing_count") |>
  mutate(missing_pct = round(missing_count / nrow(df) * 100, 2))
write_csv(missing_summary, "output/eda_missing_summary.csv")

num_summary <- df |>
  summarise(
    avg_time_taken = mean(time_taken_min, na.rm = TRUE),
    median_time_taken = median(time_taken_min, na.rm = TRUE),
    avg_age = mean(delivery_person_age, na.rm = TRUE),
    avg_rating = mean(delivery_person_ratings, na.rm = TRUE),
    avg_distance_km = mean(distance_km, na.rm = TRUE),
    avg_pickup_delay = mean(pickup_delay_min, na.rm = TRUE)
  )
write_csv(num_summary, "output/eda_numeric_summary.csv")

p1 <- ggplot(df, aes(x = time_taken_min)) +
  geom_histogram(bins = 30, fill = "#01696f", color = "white") +
  labs(title = "Distribution of Delivery Time", x = "Time Taken (min)", y = "Count") +
  theme_minimal(base_size = 13)
ggsave("output/plots/01_time_taken_distribution.png", p1, width = 8, height = 5, dpi = 150)

num_df <- df |> select(where(is.numeric)) |> select(-any_of("id"))
cor_mat <- cor(num_df, use = "pairwise.complete.obs")
cor_long <- as.data.frame(cor_mat) |> tibble::rownames_to_column("var1") |> pivot_longer(-var1, names_to = "var2", values_to = "correlation")
p2 <- ggplot(cor_long, aes(var1, var2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#01696f", midpoint = 0) +
  coord_fixed() +
  labs(title = "Correlation Heatmap", x = NULL, y = NULL, fill = "Corr") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/plots/02_correlation_heatmap.png", p2, width = 9, height = 7, dpi = 150)

p3 <- ggplot(df, aes(x = distance_km, y = time_taken_min)) +
  geom_point(alpha = 0.35, color = "#01696f") +
  geom_smooth(method = "lm", se = TRUE, color = "#a12c7b") +
  labs(title = "Delivery Time vs Distance", x = "Distance (km)", y = "Time Taken (min)") +
  theme_minimal(base_size = 13)
ggsave("output/plots/03_time_vs_distance.png", p3, width = 8, height = 5, dpi = 150)

p4 <- ggplot(df, aes(x = road_traffic_density, y = time_taken_min, fill = road_traffic_density)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.2) +
  labs(title = "Delivery Time by Traffic Density", x = "Traffic Density", y = "Time Taken (min)") +
  theme_minimal(base_size = 13) + theme(legend.position = "none")
ggsave("output/plots/04_time_by_traffic.png", p4, width = 8, height = 5, dpi = 150)

p5 <- ggplot(df, aes(x = weather_conditions, y = time_taken_min, fill = weather_conditions)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.2) +
  labs(title = "Delivery Time by Weather", x = "Weather", y = "Time Taken (min)") +
  theme_minimal(base_size = 13) + theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1))
ggsave("output/plots/05_time_by_weather.png", p5, width = 8, height = 5, dpi = 150)

p6 <- ggplot(df, aes(x = city, y = time_taken_min, fill = city)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.2) +
  labs(title = "Delivery Time by City", x = "City", y = "Time Taken (min)") +
  theme_minimal(base_size = 13) + theme(legend.position = "none")
ggsave("output/plots/06_time_by_city.png", p6, width = 7, height = 5, dpi = 150)

p7 <- ggplot(df, aes(x = festival, y = time_taken_min, fill = festival)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.2) +
  labs(title = "Delivery Time by Festival", x = "Festival", y = "Time Taken (min)") +
  theme_minimal(base_size = 13) + theme(legend.position = "none")
ggsave("output/plots/07_time_by_festival.png", p7, width = 7, height = 5, dpi = 150)

p8 <- ggplot(df, aes(x = delivery_person_age)) +
  geom_histogram(bins = 25, fill = "#01696f", color = "white") +
  labs(title = "Distribution of Age", x = "Age", y = "Count") +
  theme_minimal(base_size = 13)
ggsave("output/plots/08_age_distribution.png", p8, width = 7, height = 5, dpi = 150)

p9 <- ggplot(df, aes(x = delivery_person_ratings)) +
  geom_histogram(bins = 25, fill = "#a12c7b", color = "white") +
  labs(title = "Distribution of Ratings", x = "Ratings", y = "Count") +
  theme_minimal(base_size = 13)
ggsave("output/plots/09_rating_distribution.png", p9, width = 7, height = 5, dpi = 150)

p10 <- ggplot(df, aes(x = type_of_vehicle, y = time_taken_min, fill = type_of_vehicle)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.2) +
  labs(title = "Delivery Time by Vehicle Type", x = "Vehicle Type", y = "Time Taken (min)") +
  theme_minimal(base_size = 13) + theme(legend.position = "none")
ggsave("output/plots/10_time_by_vehicle.png", p10, width = 8, height = 5, dpi = 150)

p11 <- ggplot(df, aes(x = pickup_delay_min, y = time_taken_min)) +
  geom_point(alpha = 0.35, color = "#01696f") +
  geom_smooth(method = "lm", se = TRUE, color = "#a12c7b") +
  labs(title = "Pickup Delay vs Delivery Time", x = "Pickup Delay (min)", y = "Time Taken (min)") +
  theme_minimal(base_size = 13)
ggsave("output/plots/11_pickup_delay_vs_time.png", p11, width = 8, height = 5, dpi = 150)

p12 <- ggplot(df, aes(x = restaurant_longitude, y = restaurant_latitude)) +
  geom_point(alpha = 0.15, color = "#01696f") +
  labs(title = "Restaurant Locations", x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 13)
ggsave("output/plots/12_restaurant_locations.png", p12, width = 7, height = 5, dpi = 150)

p13 <- ggplot(df, aes(x = delivery_location_longitude, y = delivery_location_latitude)) +
  geom_point(alpha = 0.15, color = "#a12c7b") +
  labs(title = "Delivery Locations", x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 13)
ggsave("output/plots/13_delivery_locations.png", p13, width = 7, height = 5, dpi = 150)

weather_summary <- df |> group_by(weather_conditions) |> summarise(count = n(), avg_time = mean(time_taken_min, na.rm = TRUE), .groups = "drop")
city_summary <- df |> group_by(city) |> summarise(count = n(), avg_time = mean(time_taken_min, na.rm = TRUE), .groups = "drop")
write_csv(weather_summary, "output/eda_weather_summary.csv")
write_csv(city_summary, "output/eda_city_summary.csv")

cat("EDA outputs saved in output/ and output/plots/\n")
