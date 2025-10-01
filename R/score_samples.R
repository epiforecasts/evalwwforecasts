#' Metrics to use for quantile scores
#' @export
sample_metrics <- scoringutils::get_metrics(
  scoringutils::example_sample_discrete
)

#' Scores samples against observed data
#'
#' @param draws Dataframe of model draws with observed data to score
#' @param metrics Metrics to use for scoring, Default: sample_metrics
#' @param forecast_date Forecast date
#' @param offset Offset to use when transforming forecasts
#'
#' @return a dataframe containing scores for each day of forecasting horizon
#' @importFrom dplyr filter select mutate .data
#' @importFrom scoringutils as_forecast_sample transform_forecasts
#' log_shift get_metrics score
#' @importFrom rlang arg_match
score_samples <- function(
    draws,
    metrics = sample_metrics,
    forecast_date,
    offset = 1) {
  if (is.null(draws)) {
    scores <- NULL
  } else {
    # Filter to after the last date
    forecasted_draws <- draws |>
      dplyr::filter(.data$date >= !!forecast_date) |>
      dplyr::select(
        model = "model",
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

    if (is.null(metrics)) {
      metrics <- get_metrics(to_score)
    }

    scores <- score(to_score, metrics = metrics) |>
      dplyr::mutate(
        period = ifelse(
          .data$date <= .data$forecast_date,
          "estimate",
          "forecast"
        )
      )
  }

  return(scores)
}
