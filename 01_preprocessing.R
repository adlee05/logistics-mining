pkgs <- c("tidyverse", "lubridate", "janitor", "geosphere")
installed <- rownames(installed.packages())
for (p in pkgs) if (!p %in% installed) install.packages(p, quiet = TRUE)

library(tidyverse)
library(lubridate)
library(janitor)
library(geosphere)

dir.create("output", showWarnings = FALSE)

# ----------------------------
# 1. Load raw data
# ----------------------------
df <- read_csv(
  "./datasets/Zomato_Dataset.csv",
  na = c("", "NA", "NaN", " NaN", "NaN "),
  show_col_types = FALSE
) |>
  clean_names()

cat("Raw data loaded:", nrow(df), "rows x", ncol(df), "cols\n")

# ----------------------------
# 2. Clean & convert columns
# ----------------------------
df <- df |>
  mutate(
    across(where(is.character), ~ str_trim(.x)),
    delivery_person_age = as.numeric(delivery_person_age),
    delivery_person_ratings = as.numeric(delivery_person_ratings),
    multiple_deliveries = as.numeric(multiple_deliveries),
    vehicle_condition = as.numeric(vehicle_condition),
    time_taken_min = as.numeric(str_extract(time_taken_min, "\\d+")),
    order_date = dmy(order_date),
    time_orderd = parse_date_time(time_orderd, orders = c("H:M:S", "H:M", "I:M:S p", "I:M p")),
    time_order_picked = parse_date_time(time_order_picked, orders = c("H:M:S", "H:M", "I:M:S p", "I:M p"))
  ) |>
  distinct()

cat("After type conversion and dedup:", nrow(df), "rows\n")

# ----------------------------
# 3. Handle missing target
# ----------------------------
df <- df |> filter(!is.na(time_taken_min))

# ----------------------------
# 4. Missing value imputation
# ----------------------------
mode_val <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

df <- df |>
  mutate(
    delivery_person_age = ifelse(is.na(delivery_person_age), median(delivery_person_age, na.rm = TRUE), delivery_person_age),
    delivery_person_ratings = ifelse(is.na(delivery_person_ratings), median(delivery_person_ratings, na.rm = TRUE), delivery_person_ratings),
    multiple_deliveries = ifelse(is.na(multiple_deliveries), median(multiple_deliveries, na.rm = TRUE), multiple_deliveries),
    vehicle_condition = ifelse(is.na(vehicle_condition), median(vehicle_condition, na.rm = TRUE), vehicle_condition)
  )

cat_cols <- c("weather_conditions", "road_traffic_density", "type_of_order", "type_of_vehicle", "festival", "city")
for (col in cat_cols) {
  df[[col]][is.na(df[[col]])] <- mode_val(df[[col]])
}

# ----------------------------
# 5. Remove outliers (IQR)
# ----------------------------
remove_outliers_iqr <- function(data, col) {
  q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
  q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  data |> filter(.data[[col]] >= q1 - 1.5 * iqr & .data[[col]] <= q3 + 1.5 * iqr)
}

df <- df |>
  remove_outliers_iqr("time_taken_min") |>
  remove_outliers_iqr("delivery_person_age") |>
  remove_outliers_iqr("delivery_person_ratings")

cat("After outlier removal:", nrow(df), "rows\n")

# ----------------------------
# 6. Feature engineering
# ----------------------------
df <- df |>
  rowwise() |>
  mutate(
    distance_km = distHaversine(
      c(restaurant_longitude, restaurant_latitude),
      c(delivery_location_longitude, delivery_location_latitude)
    ) / 1000
  ) |>
  ungroup() |>
  mutate(
    pickup_delay_min = as.numeric(difftime(time_order_picked, time_orderd, units = "mins")),
    pickup_delay_min = ifelse(is.na(pickup_delay_min), NA, ifelse(pickup_delay_min < 0, pickup_delay_min + 1440, pickup_delay_min)),
    order_hour = hour(time_orderd),
    order_day_of_week = wday(order_date, label = TRUE),
    order_month = month(order_date, label = TRUE),
    is_weekend = ifelse(wday(order_date) %in% c(1, 7), 1L, 0L),
    age_group = cut(
      delivery_person_age,
      breaks = c(15, 20, 25, 30, 35, 40, Inf),
      labels = c("15-20", "21-25", "26-30", "31-35", "36-40", "40+"),
      right = TRUE
    ),
    is_rush_hour = ifelse(
      (order_hour >= 7 & order_hour <= 9) |
        (order_hour >= 12 & order_hour <= 14) |
        (order_hour >= 18 & order_hour <= 21),
      1L, 0L
    ),
    road_traffic_density_ord = as.integer(
      factor(road_traffic_density, levels = c("Low", "Medium", "High", "Jam"))
    ),
    weather_conditions = as.factor(weather_conditions),
    type_of_order = as.factor(type_of_order),
    type_of_vehicle = as.factor(type_of_vehicle),
    festival = as.factor(festival),
    city = as.factor(city)
  )

# Optional: impute missing pickup delay with median after parsing
if (any(is.na(df$pickup_delay_min))) {
  df <- df |>
    mutate(
      pickup_delay_min = ifelse(
        is.na(pickup_delay_min),
        median(pickup_delay_min, na.rm = TRUE),
        pickup_delay_min
      )
    )
}

# ----------------------------
# 7. Save cleaned data
# ----------------------------
write_csv(df, "output/food_delivery_clean.csv")

cat("Saved cleaned file: output/food_delivery_clean.csv\n")
cat("Final shape:", nrow(df), "rows x", ncol(df), "cols\n")
cat("Done.\n")