#' Metrics to use for sample scores
#' @export
sample_metrics <- scoringutils::get_metrics(
  scoringutils::example_sample_discrete
)

#' Metrics to use for quantile scores
#' @export
sample_metrics_quantiles <- scoringutils::get_metrics(
  scoringutils::example_quantile
)

#' Preparing draws for scoring, including choosing quantiles or samples
#'
#' @param draws Dataframe of model draws with observed data to score
#' @param forecast_date Forecast data
#' @param offset Offset to use when transforming forecasts
#' @param quantiles Converts to quantiles if TRUE, otherwise leaves as samples
#'
#' @returns Dataframe of samples/quantiles, on log scale
draws_for_scoring <- function(
    draws,
    forecast_date,
    offset = 1,
    quantiles = FALSE) {
  if (is.null(draws)) {
    to_score <- NULL
  } else {
    # Filter to after the last date
    forecasted_draws <- draws |>
      filter(.data$date >= as.Date(forecast_date)) |>
      select(
        "model",
        "include_ww",
        "location",
        "forecast_date",
        "date",
        "value",
        "eval_data",
        "draw"
      )

    to_score <- forecasted_draws |>
      as_forecast_sample(
        predicted = "value",
        observed = "eval_data",
        sample_id = "draw"
      ) |>
      transform_forecasts(
        fun = log_shift,
        offset = offset
      )

    if (isTRUE(quantiles)) {
      to_score <- to_score |>
        as_forecast_quantile(
          probs = c(0.05, 0.25, 0.5, 0.75, 0.95)
        )
    }
  }

  return(to_score)
}


#' Scores samples against observed data
#'
#' @param draws_for_scoring Dataframe of samples/quantiles on log scale
#' @param metrics Metrics to use for scoring, Default: sample_metrics
#'
#' @return a dataframe containing scores for each day of forecasting horizon
#' @importFrom dplyr filter select mutate
#' @importFrom scoringutils as_forecast_sample transform_forecasts
#' @importFrom scoringutils log_shift get_metrics score
#' @importFrom rlang .data
generate_scores <- function(
    draws_for_scoring,
    metrics = sample_metrics_quantiles) {
  if (is.null(draws_for_scoring)) {
    scores <- NULL
  } else {
    if (is.null(metrics)) {
      metrics <- get_metrics(to_score)
    }

    scores <- score(draws_for_scoring, metrics = metrics) |>
      mutate(
        period = ifelse(
          .data$date <= .data$forecast_date,
          "estimate",
          "forecast"
        )
      )

    # Keep scores on log scale only
    scores <- filter(scores, scale == "log")
  }

  return(scores)
}
