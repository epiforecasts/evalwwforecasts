#' Metrics to use for quantile scores
#' @export
sample_metrics <- scoringutils::get_metrics(
  scoringutils::example_sample_discrete
)

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
        "draw",
      )
    to_score <- forecasted_draws |>
      scoringutils::as_forecast_sample(
        predicted = "value",
        observed = "eval_data",
        sample_id = "draw"
      ) |>
      scoringutils::transform_forecasts(
        fun = scoringutils::log_shift,
        offset = offset
      )

    if (is.null(metrics)) {
      metrics <- scoringutils::get_metrics(to_score)
    }

    scores <- scoringutils::score(to_score, metrics = metrics) |>
      dplyr::mutate(
        period = ifelse(
          .data$date <= .data$forecast_date,
          "estimate",
          "forecast"
        ),
      )
  }

  return(scores)
}
