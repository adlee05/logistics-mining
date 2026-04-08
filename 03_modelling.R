pkgs <- c("tidyverse", "janitor", "caret", "rpart", "Metrics", "ggplot2")
installed <- rownames(installed.packages())
for (p in pkgs) {
  if (!p %in% installed) install.packages(p, quiet = TRUE)
}

library(tidyverse)
library(janitor)
library(caret)
library(rpart)
library(Metrics)
library(ggplot2)

set.seed(42)
dir.create("output", showWarnings = FALSE)
dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 1. Load cleaned data
# ============================================================
df <- read_csv("output/food_delivery_clean.csv", show_col_types = FALSE) |>
  clean_names()

cat("Loaded cleaned data:", nrow(df), "rows x", ncol(df), "cols\n")

# ============================================================
# 2. Select modelling features
# ============================================================
model_df <- df |>
  select(
    time_taken_min,
    distance_km,
    pickup_delay_min,
    order_hour,
    is_weekend,
    is_rush_hour,
    delivery_person_age,
    delivery_person_ratings,
    multiple_deliveries,
    vehicle_condition,
    road_traffic_density_ord,
    weather_conditions,
    type_of_order,
    type_of_vehicle,
    festival,
    city
  )

# ============================================================
# 3. One-hot encode categorical variables
# ============================================================
dummies <- dummyVars(
  ~ weather_conditions + type_of_order + type_of_vehicle + festival + city,
  data = model_df,
  fullRank = TRUE
)

encoded_cats <- predict(dummies, newdata = model_df) |>
  as.data.frame()

model_df_encoded <- model_df |>
  select(-weather_conditions, -type_of_order, -type_of_vehicle, -festival, -city) |>
  bind_cols(encoded_cats) |>
  drop_na()

cat("Final modelling rows after cleanup:", nrow(model_df_encoded), "\n")

# ============================================================
# 4. Train / Test split
# ============================================================
train_idx <- createDataPartition(model_df_encoded$time_taken_min, p = 0.8, list = FALSE)
train_df <- model_df_encoded[train_idx, ]
test_df  <- model_df_encoded[-train_idx, ]

X_train <- train_df |>
  select(-time_taken_min)

y_train <- train_df$time_taken_min

X_test <- test_df |>
  select(-time_taken_min)

y_test <- test_df$time_taken_min

cat("Train rows:", nrow(train_df), "| Test rows:", nrow(test_df), "\n")

# ============================================================
# 5. Cross-validation setup
# ============================================================
cv_ctrl <- trainControl(method = "cv", number = 5)

# ============================================================
# 6. Linear Regression
# ============================================================
cat("\nTraining Linear Regression...\n")
lm_model <- train(
  x = X_train,
  y = y_train,
  method = "lm",
  trControl = cv_ctrl
)

lm_pred <- predict(lm_model, newdata = X_test)
lm_rmse <- rmse(y_test, lm_pred)
lm_mae  <- mae(y_test, lm_pred)
lm_r2   <- cor(y_test, lm_pred)^2

cat("Linear Regression -> RMSE:", round(lm_rmse, 3),
    "| MAE:", round(lm_mae, 3),
    "| R2:", round(lm_r2, 3), "\n")

# ============================================================
# 7. Decision Tree
# ============================================================
cat("\nTraining Decision Tree...\n")
dt_model <- train(
  x = X_train,
  y = y_train,
  method = "rpart",
  trControl = cv_ctrl,
  tuneLength = 10
)

dt_pred <- predict(dt_model, newdata = X_test)
dt_rmse <- rmse(y_test, dt_pred)
dt_mae  <- mae(y_test, dt_pred)
dt_r2   <- cor(y_test, dt_pred)^2

cat("Decision Tree -> RMSE:", round(dt_rmse, 3),
    "| MAE:", round(dt_mae, 3),
    "| R2:", round(dt_r2, 3), "\n")

# ============================================================
# 8. Model comparison table
# ============================================================
results_df <- tibble(
  Model = c("Linear Regression", "Decision Tree"),
  RMSE  = round(c(lm_rmse, dt_rmse), 3),
  MAE   = round(c(lm_mae, dt_mae), 3),
  R2    = round(c(lm_r2, dt_r2), 3)
)

print(results_df)
write_csv(results_df, "output/model_comparison.csv")

# ============================================================
# 9. Actual vs Predicted plots
# ============================================================
p_lm <- ggplot(
  tibble(Actual = y_test, Predicted = lm_pred),
  aes(x = Actual, y = Predicted)
) +
  geom_point(alpha = 0.35, color = "#01696f") +
  geom_abline(slope = 1, intercept = 0, color = "#a12c7b", linetype = "dashed") +
  labs(
    title = "Linear Regression: Actual vs Predicted",
    x = "Actual",
    y = "Predicted"
  ) +
  theme_minimal(base_size = 13)

ggsave("output/plots/15_lm_actual_vs_predicted.png", p_lm, width = 8, height = 6, dpi = 150)

p_dt <- ggplot(
  tibble(Actual = y_test, Predicted = dt_pred),
  aes(x = Actual, y = Predicted)
) +
  geom_point(alpha = 0.35, color = "#d19900") +
  geom_abline(slope = 1, intercept = 0, color = "#a12c7b", linetype = "dashed") +
  labs(
    title = "Decision Tree: Actual vs Predicted",
    x = "Actual",
    y = "Predicted"
  ) +
  theme_minimal(base_size = 13)

ggsave("output/plots/16_dt_actual_vs_predicted.png", p_dt, width = 8, height = 6, dpi = 150)

# ============================================================
# 10. Residual plots
# ============================================================
p_lm_resid <- ggplot(
  tibble(Predicted = lm_pred, Residual = y_test - lm_pred),
  aes(x = Predicted, y = Residual)
) +
  geom_point(alpha = 0.35, color = "#01696f") +
  geom_hline(yintercept = 0, color = "#a12c7b", linetype = "dashed") +
  labs(
    title = "Residual Plot - Linear Regression",
    x = "Predicted",
    y = "Residual"
  ) +
  theme_minimal(base_size = 13)

ggsave("output/plots/17_lm_residual_plot.png", p_lm_resid, width = 8, height = 5, dpi = 150)

p_dt_resid <- ggplot(
  tibble(Predicted = dt_pred, Residual = y_test - dt_pred),
  aes(x = Predicted, y = Residual)
) +
  geom_point(alpha = 0.35, color = "#d19900") +
  geom_hline(yintercept = 0, color = "#a12c7b", linetype = "dashed") +
  labs(
    title = "Residual Plot - Decision Tree",
    x = "Predicted",
    y = "Residual"
  ) +
  theme_minimal(base_size = 13)

ggsave("output/plots/18_dt_residual_plot.png", p_dt_resid, width = 8, height = 5, dpi = 150)

# ============================================================
# 11. RMSE comparison plot
# ============================================================
p_rmse <- ggplot(results_df, aes(x = reorder(Model, RMSE), y = RMSE, fill = Model)) +
  geom_col(alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Model Comparison - RMSE",
    x = NULL,
    y = "RMSE"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave("output/plots/19_model_rmse_comparison.png", p_rmse, width = 8, height = 4, dpi = 150)

# ============================================================
# 12. Linear model coefficients
# ============================================================
coef_df <- coef(lm_model$finalModel) |>
  as.data.frame() |>
  tibble::rownames_to_column("Feature")
colnames(coef_df)[2] <- "Coefficient"
write_csv(coef_df, "output/lm_coefficients.csv")

cat("\nSaved simplified modelling outputs in output/ and output/plots/\n")