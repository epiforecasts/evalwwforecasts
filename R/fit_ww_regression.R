#' Fit a direct quantile regression of hospital admissions on recent trends
#'
#' A simple statistical forecasting model that uses wastewater without a
#' mechanistic model and without projecting wastewater forward. For each
#' horizon `h` (days after the forecast date) a separate quantile regression
#' is fitted ("direct" multi-step forecasting, as in e.g.
#' `epipredict::arx_forecaster()`):
#'
#' `log(y[s + h] + 1) - log(y[s - lag_hosp] + 1) ~ hosp_growth(s) +
#' ww_growth(s)`
#'
#' where `y` is the 7-day rolling sum of hospital admissions, `s` is a
#' (pseudo) forecast date, `hosp_growth(s)` is the change in log admissions
#' over the `hosp_growth_window` days up to the latest admissions value
#' available at `s`, and `ww_growth(s)` is the wastewater growth rate at `s`
#' from `get_ww_growth()`: a population-weighted linear regression of log
#' concentration on time, with site-lab fixed effects, using all samples
#' collected in the `ww_window` days before `s` and reported by `s`.
#'
#' The training set uses every past date `s` for which `y[s + h]` is
#' observed in the data available on the forecast date. To mimic what was
#' known in real time, the admissions are lagged by the lag on the forecast
#' date, and each wastewater site's samples are only used if they are dated
#' at least that site's lag on the forecast date before `s`. With
#' `include_ww = FALSE` the wastewater term is dropped.
#'
#' @inheritParams fit_arima
#' @param ww_data_for_fit Data.frame of wastewater data as of the forecast
#'   date, as returned by `get_ww_as_of_forecast_date()`. Ignored if
#'   `include_ww = FALSE`.
#' @param quantiles Numeric vector of quantile levels to forecast.
#' @param ww_window Integer, number of days before each (pseudo) forecast
#'   date from which wastewater samples are used. Default `35`, which with
#'   the typical reporting lag of about 12 days leaves about 3 weeks of
#'   samples.
#' @param min_ww_span Integer, minimum number of days the wastewater
#'   samples must span for the growth rate to be estimated. Default `14`.
#'   If the growth rate cannot be estimated on the forecast date, the
#'   forecast is made without the wastewater term.
#' @param hosp_growth_window Integer, number of days over which the change
#'   in log admissions is computed. Default `7`.
#' @param training_period Integer, number of days of past forecast dates to
#'   train on, or `NULL` (default) to use all available.
#'
#' @returns Data.frame of forecasts in wide format, with the same columns as
#'   `fit_arima()`
#' @importFrom dplyr filter mutate select left_join group_by summarise
#'   bind_rows arrange
#' @importFrom lubridate ymd days
#' @autoglobal
fit_ww_regression <- function(hosp_data_for_fit,
                              hosp_data_for_eval,
                              ww_data_for_fit,
                              forecast_date,
                              data_right_trunc,
                              include_ww,
                              model,
                              quantiles,
                              forecast_horizon = 28,
                              ww_window = 35,
                              min_ww_span = 14,
                              hosp_growth_window = 7,
                              training_period = NULL) {
  if (!requireNamespace("quantreg", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg quantreg} is needed for this model.")
  }
  forecast_date <- ymd(forecast_date)
  hosp <- hosp_data_for_fit |>
    filter(!is.na(updated_hosp_7d_count)) |>
    mutate(date = as.Date(date)) |>
    arrange(date)
  last_hosp_date <- max(hosp$date)
  lag_hosp <- as.integer(forecast_date - last_hosp_date)
  log_y <- stats::setNames(
    log(hosp$updated_hosp_7d_count + 1),
    as.character(hosp$date)
  )
  get_log_y <- function(dates) unname(log_y[as.character(dates)])

  # Pseudo forecast dates: every date with the admissions history needed
  first_origin <- min(hosp$date) + lag_hosp + hosp_growth_window
  if (!is.null(training_period)) {
    first_origin <- max(first_origin, forecast_date - training_period)
  }
  origins <- seq(first_origin, forecast_date, by = "day")
  features <- data.frame(origin = origins) |>
    mutate(
      anchor = get_log_y(origin - lag_hosp),
      hosp_growth = anchor -
        get_log_y(origin - lag_hosp - hosp_growth_window)
    )

  if (isTRUE(include_ww)) {
    ww <- ww_data_for_fit |>
      mutate(
        date = as.Date(date),
        lab = as.character(lab),
        site = as.character(site)
      ) |>
      filter(date <= .env$forecast_date)
    # Each site's reporting lag on the forecast date, applied to every
    # training date so the training data have the same sites and staleness
    site_lags <- ww |>
      group_by(site) |>
      summarise(site_lag = as.integer(.env$forecast_date - max(date)))
    ww <- left_join(ww, site_lags, by = "site")
    features$ww_growth <- vapply(
      features$origin,
      function(s) {
        get_ww_growth(ww,
          origin = s, window = ww_window, min_span = min_ww_span
        )
      },
      numeric(1)
    )
    reg_formula <- response ~ hosp_growth + ww_growth
    if (is.na(features$ww_growth[features$origin == forecast_date])) {
      cli::cli_inform(c(
        "!" = "Wastewater growth rate could not be estimated on
        {forecast_date}, forecasting without wastewater."
      ))
      features$ww_growth <- NULL
      reg_formula <- response ~ hosp_growth
    }
  } else {
    reg_formula <- response ~ hosp_growth
  }

  newdata <- filter(features, origin == forecast_date)
  if (nrow(newdata) != 1 || anyNA(newdata)) {
    cli::cli_abort(
      "Features are missing on the forecast date {forecast_date}."
    )
  }
  train_features <- filter(features, origin < forecast_date)

  forecasts <- lapply(0:forecast_horizon, function(h) {
    train <- train_features |>
      filter(origin + h <= last_hosp_date) |>
      mutate(response = get_log_y(origin + h) - anchor) |>
      filter(!is.na(response), !is.na(hosp_growth))
    if ("ww_growth" %in% names(train)) {
      train <- filter(train, !is.na(ww_growth))
    }
    fit <- quantreg::rq(reg_formula, tau = quantiles, data = train)
    pred <- as.numeric(stats::predict(fit, newdata = newdata))
    # Avoid quantile crossing
    pred <- sort(pred)
    pred_y <- pmax(exp(newdata$anchor + pred) - 1, 0)
    data.frame(
      date = forecast_date + h,
      t(stats::setNames(pred_y, paste0("q_", quantiles)))
    )
  })
  forecast_df <- bind_rows(forecasts)
  names(forecast_df) <- c("date", paste0("q_", quantiles))

  hosp_data_eval_forecast <- hosp_data_for_eval |>
    filter(
      date >= .env$forecast_date,
      date <= .env$forecast_date + days(forecast_horizon)
    )
  hosp_data_real_time <- unique(hosp_data_for_fit$hosp_data_real_time)
  forecast_df <- forecast_df |>
    mutate(
      data_right_trunc = data_right_trunc,
      include_ww = include_ww,
      model = model
    ) |>
    left_join(hosp_data_eval_forecast, by = "date") |>
    mutate(
      forecast_date = forecast_date,
      hosp_data_real_time = hosp_data_real_time
    )
  return(forecast_df)
}

#' Wastewater growth rate as known on a (pseudo) forecast date
#'
#' Fits a linear regression of log concentration on date with a fixed
#' effect for each site and lab, so that differences in level between
#' sites and changes of lab do not affect the slope, which is estimated
#' only from changes within each site-lab series. Uses the samples dated
#' in the `window` days before `origin` that were reported by `origin`,
#' i.e. dated at least `site_lag` days before it. Each sample is weighted
#' by its site's population.
#'
#' @param ww Data.frame with `date`, `site`, `lab`, `site_pop`,
#'   `site_lag` and `log_genome_copies_per_ml`
#' @param origin Date, the (pseudo) forecast date
#' @param window Integer, number of days before `origin` to use samples from
#' @param min_span Integer, minimum number of days between the first and
#'   last sample used
#'
#' @returns Numeric scalar, the slope in log concentration per day, or `NA`
#'   if no site-lab has samples on at least two dates or the samples span
#'   fewer than `min_span` days
#' @importFrom stats coef lm
#' @autoglobal
get_ww_growth <- function(ww, origin, window = 35, min_span = 14) {
  d <- ww[ww$date > origin - window & ww$date <= origin - ww$site_lag, ]
  if (nrow(d) == 0) {
    return(NA_real_)
  }
  n_dates <- tapply(d$date, d$lab, function(x) length(unique(x)))
  span <- as.numeric(max(d$date) - min(d$date))
  if (!any(n_dates >= 2) || span < min_span) {
    return(NA_real_)
  }
  d$t <- as.numeric(d$date - origin)
  fit <- if (length(unique(d$lab)) > 1) {
    lm(log_genome_copies_per_ml ~ t + lab, data = d, weights = site_pop)
  } else {
    lm(log_genome_copies_per_ml ~ t, data = d, weights = site_pop)
  }
  return(unname(coef(fit)["t"]))
}
