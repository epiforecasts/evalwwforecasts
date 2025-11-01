#' Metrics to use for sample scores
#' @export
sample_metrics <- scoringutils::get_metrics(
  scoringutils::example_sample_discrete
)

#' Metrics to use for quantile scores
#' @export
quantile_metrics <- scoringutils::get_metrics(
  scoringutils::example_quantile
)

#' Preparing draws for scoring, including choosing quantiles or samples
#'
#' @param draws Dataframe of model draws with observed data to score
#' @param forecast_date Forecast date
#' @param offset Offset to use when transforming forecasts
#' @param quantiles Converts to quantiles if TRUE, otherwise leaves as samples
#' @param probs Numeric vector of quantile probabilities to use
#' when \code{quantiles = TRUE}
#' @param remove_before_forecast_date Boolean indicating whether or not
#' to remove the values before the forecast date, default is FALSE.
#' @returns A forecast_sample or forecast_quantile object (depending on
#'   the quantiles parameter) on the log scale, ready for scoring with
#'   generate_scores(). Returns NULL if draws is NULL.
#'
#' @importFrom dplyr filter select
#' @importFrom scoringutils as_forecast_sample transform_forecasts
#' @importFrom scoringutils log_shift as_forecast_quantile
#' @importFrom rlang .data
draws_for_scoring <- function(
    draws,
    forecast_date,
    offset = 1,
    quantiles = FALSE,
    remove_before_forecast_date = FALSE,
    probs = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  if (is.null(draws)) {
    to_score <- NULL
  } else {
    if (remove_before_forecast_date) {
      draws <- draws |> filter(date >= as.Date(forecast_date))
    }
    forecasted_draws <- draws |>
      select(
        "model",
        "include_ww",
        "location",
        "forecast_date",
        "hosp_data_real_time",
        "date",
        "pred_value7dsum",
        "updated_hosp_7d_count",
        "draw"
      ) |>
      filter(!is.na(pred_value7dsum))

    to_score <- forecasted_draws |>
      as_forecast_sample(
        forecast_unit = c(
          "model", "include_ww", "hosp_data_real_time",
          "location", "forecast_date", "date"
        ),
        predicted = "pred_value7dsum",
        observed = "updated_hosp_7d_count",
        sample_id = "draw"
      ) |>
      transform_forecasts(
        fun = log_shift,
        offset = offset
      )

    if (isTRUE(quantiles)) {
      to_score <- as_forecast_quantile(
        to_score,
        probs = probs
      )
    }
  }

  return(to_score)
}

#' Format the baseline forecasts to match the outputs from
#' wwinference
#'
#' @param baseline_forecasts Data.frame of quantiled baseline forecasts
#' @param quantiles_to_save Vector of quantiles to save
#' @inheritParams draws_for_scoring
#' @param fp_data Character string indicating the high level file path to
#'   save the data.
#' @importFrom tidyr pivot_longer starts_with
#' @importFrom cli cli_warn
#' @importFrom fs dir_create
#' @returns Data.frame with the baseline forecasts formatted for scoringutils
#' @autoglobal
format_baseline_forecasts <- function(baseline_forecasts,
                                      quantiles_to_save,
                                      offset = 1,
                                      fp_data = "output") {
  loc <- unique(baseline_forecasts$state)
  forecast_date <- unique(baseline_forecasts$forecast_date)
  # pivot quantiles from wide to long
  bl_to_score <- baseline_forecasts |>
    tidyr::pivot_longer(
      cols = starts_with("q_"),
      names_prefix = "q_",
      values_to = "pred_value7dsum",
      names_to = "quantile_level"
    ) |>
    dplyr::rename(
      location = state
    ) |>
    select(
      "model",
      "include_ww",
      "location",
      "forecast_date",
      "date",
      "hosp_data_real_time",
      "pred_value7dsum",
      "updated_hosp_7d_count",
      "quantile_level"
    ) |>
    filter(quantile_level %in% quantiles_to_save) |>
    mutate(quantile_level = as.numeric(quantile_level)) |>
    mutate(pred_value7dsum = pmax(pred_value7dsum, 0)) |> # Also hacky solution
    as_forecast_quantile(
      forecast_unit = c(
        "model", "include_ww",
        "location", "forecast_date", "hosp_data_real_time",
        "date"
      ),
      predicted = "pred_value7dsum",
      observed = "updated_hosp_7d_count"
    ) |>
    transform_forecasts(
      fun = log_shift,
      offset = offset
    )

  if (!all(unique(bl_to_score$quantile_level) %in% quantiles_to_save)) {
    cli_warn(
      message = "Baseline forecasts don't contain all the quantiles needed."
    )
  }
  full_fp <- file.path(fp_data, forecast_date, loc, "data")
  if (!file.exists(file.path(full_fp))) {
    dir_create(full_fp, recursive = TRUE, showWarnings = FALSE)
  }
  write_csv(
    bl_to_score,
    file.path(
      full_fp,
      "baseline_quantiles_rt_{hosp_data_real_time}.csv"
    )
  )
  return(bl_to_score)
}


#' Scores samples against observed data
#'
#' @param draws_for_scoring Dataframe of samples/quantiles on log scale
#' @param metrics Metrics to use for scoring. Default is NULL which will
#'   get metrics from scoringutils function.
#' @param scale_selected Character string indicating whether to score
#'   on natural or log scale, default is "log".
#' @param save_scores Boolean indicating whether or not to save the scores.
#' @param fp_data Character string indicating file path to save the scores.
#'   Default is NULL.
#' @return a dataframe containing scores for each day of forecasting horizon
#' @importFrom dplyr filter select mutate
#' @importFrom lubridate ymd
#' @importFrom scoringutils as_forecast_sample transform_forecasts log_shift
#'   get_metrics score
#' @importFrom rlang .data
generate_scores <- function(
    draws_for_scoring,
    metrics = NULL,
    scale_selected = "log",
    save_scores = FALSE,
    fp_data = NULL) {
  if (is.null(draws_for_scoring)) {
    scores <- NULL
  } else {
    if (is.null(metrics)) {
      metrics <- get_metrics(draws_for_scoring)
    }

    scores <- score(draws_for_scoring, metrics = metrics) |>
      mutate(
        horizon = as.numeric(ymd(date) - ymd(forecast_date))
      ) |>
      filter(scale == scale_selected)
  }
  if (isTRUE(save_scores)) {
    if (!file.exists(fp_data)) {
      dir_create(fp_data, recursive = TRUE, showWarnings = FALSE)
    }
    write_csv(scores, file.path(fp_data, "scores.csv"))
  }

  return(scores)
}
