library(forecast)
library(lubridate)
library(targets)
library(dplyr)
library(zoo)
library(ggplot2)

tar_load(ww_data)
tar_load(hosp_data)

forecast_date <- "2025-04-01"
start_date <- ymd(forecast_date) - days(90)

# Get the data from one location and one site for now
hosp_data1 <- hosp_data |>
  filter(
    state_pop == 3755251,
    date <= forecast_date,
    date > start_date
  ) |>
  distinct()
ww_data1 <- ww_data |>
  filter(
    site == "Berlin Ruhleben",
    date <= forecast_date,
    date > start_date
  ) |>
  distinct()

date_spine <- tibble(date = seq(
  from = ymd(start_date) + days(1),
  to = ymd(forecast_date),
  by = "days"
))

data <- date_spine |>
  left_join(hosp_data1, by = "date") |>
  left_join(ww_data1, by = "date") |>
  distinct() |>
  mutate(cont_ww = na.interp(log_genome_copies_per_ml))

ggplot(data) +
  geom_line(aes(x = date, y = cont_ww)) +
  geom_point(aes(x = date, y = log_genome_copies_per_ml))

ggplot(data) +
  geom_line(aes(x = date, y = daily_hosp_admits)) +
  geom_point(aes(x = date, y = daily_hosp_admits))

ggplot(data) +
  geom_line(aes(x = date, y = updated_hosp_7d_count)) +
  geom_point(aes(x = date, y = updated_hosp_7d_count))

med_hosp <- median(data$updated_hosp_7d_count)
med_ww <- median(data$cont_ww)

auto_arimax_model <- auto.arima(data$updated_hosp_7d_count,
  xreg = data$cont_ww,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

# Display model results
print("Auto-ARIMA Model Results:")

# Extract key information

message("p =", auto_arimax_model$arma[1]) # AR order
message("d =", auto_arimax_model$arma[6]) # Differencing order
message("q =", auto_arimax_model$arma[2]) # MA order
message("\nBeta (coefficient on X) =", coef(auto_arimax_model)["xreg"])


plot_results <- function(model, y, x) {
  fitted_values <- fitted(model)
  residuals_vals <- residuals(model)

  par(mfrow = c(2, 2))

  # Original series vs fitted
  plot(y, main = "Original vs Fitted Values", ylab = "Y")
  lines(fitted_values, col = "red")
  legend("topleft", c("Original", "Fitted"), col = c("black", "red"), lty = 1)

  # Residuals
  plot(residuals_vals, main = "Residuals", ylab = "Residuals")
  abline(h = 0, col = "red", lty = 2)

  # QQ plot
  qqnorm(residuals_vals, main = "Q-Q Plot of Residuals")
  qqline(residuals_vals, col = "red")

  # Exogenous variable
  plot(x, main = "Exogenous Variable (X)", ylab = "X")
}

# Generate plots
plot_results(auto_arimax_model, data$updated_hosp_7d_count, data$cont_ww)

forecast_result_x <- forecast(auto_arimax_model, h = 28)

forecast_df <- data.frame(
  date = seq(from = ymd(forecast_date), to = ymd(forecast_date) + days(27), by = "days"),
  point_forecast_x = as.numeric(forecast_result_x$mean),
  lower_80_x = as.numeric(forecast_result_x$lower[, "80%"]),
  upper_80_x = as.numeric(forecast_result_x$upper[, "80%"]),
  lower_95_x = as.numeric(forecast_result_x$lower[, "95%"]),
  upper_95_x = as.numeric(forecast_result_x$upper[, "95%"])
)




# Repeat without wastewater (ARIMA model)--------------------------

auto_arima_model <- auto.arima(data$updated_hosp_7d_count,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

# Extract key information

message("p =", auto_arima_model$arma[1]) # AR order
message("d =", auto_arima_model$arma[6]) # Differencing order
message("q =", auto_arima_model$arma[2]) # MA order




forecast_result <- forecast(auto_arima_model, h = 28)

# Add to the forecast df
forecast_df <- forecast_df |>
  mutate(
    point_forecast = as.numeric(forecast_result$mean),
    lower_80 = as.numeric(forecast_result$lower[, "80%"]),
    upper_80 = as.numeric(forecast_result$upper[, "80%"]),
    lower_95 = as.numeric(forecast_result$lower[, "95%"]),
    upper_95 = as.numeric(forecast_result$upper[, "95%"])
  )



ggplot() +
  geom_line(
    data = data,
    aes(x = date, y = updated_hosp_7d_count), color = "blue"
  ) +
  geom_line(
    data = forecast_df,
    aes(x = date, y = point_forecast), color = "black"
  ) +
  geom_line(
    data = forecast_df,
    aes(x = date, y = point_forecast_x), color = "darkgreen"
  )
