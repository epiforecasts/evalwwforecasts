#' Suppress output and messages for code.
#' @param code Code to run quietly.
#' @return The result of running the code.
#' @export
#' @examples
#' result <- quiet(message("This message should be suppressed"))
#' print(result)
quiet <- function(code) {
  sink(nullfile())
  on.exit(sink())
  return(suppressMessages(code))
}

#' Save a dataframe to a csv and return the path for targets
#' @param df dataframe to save
#' @param filename name of dataframe
#' @param path directory to save file in
#' @param allow_empty Boolean indicating whether to save if df is empty,
#'    Default is `TRUE`.
#' @importFrom readr write_csv
#' @export
save_csv <- function(df, filename, path, allow_empty = TRUE) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(path, filename)

  if (allow_empty || nrow(df) > 0) {
    write_csv(df, path)
  }
  return(path)
}

#' Aggregate individual trajectory
#' timeseries or forecasts to quantile
#' timeseries or forecasts
#'
#' Given a tidy data frame of
#' trajectories, aggregate it to
#' a quantile timeseries for the
#' given quantile values.
#'
#' From https://github.com/CDCgov/forecasttools/blob/main/R/trajectories_to_quantiles.R #nolint
#'
#' @param trajectories Tidy data frame or tibble
#' of trajectories
#' @param quantiles Quantiles to output for each
#' timepoint (default the FluSight/COVIDHub 2024-25 quantiles:
#' `c(0.01, 0.025, 1:19/20, 0.975, 0.99)`
#' @param timepoint_cols Name(s) of the column(s) in `trajectories`
#' that identifies unique timepoints. Default `"timepoint"`.
#' @param value_col name of the column in `trajectories`
#' with the trajectory values (for which we wish to
#' compute quantiles), e.g. `hosp`, `weekly_hosp`, `cases`,
#' etc. Default `value`.
#' @param quantile_value_name What to name
#' the column containing quantile values in
#' the output table. Default `"quantile_value"`
#' @param quantile_level_name What to name
#' the column containing quantile levels in
#' the output table. Default `"quantile_level"`
#' @param id_cols additional id columns in
#' `trajectories` to group by before aggregating,
#' e.g. a `location` column if `trajectories` contains
#' trajectories over the same time period for multiple
#' locations, such as different US States and Territories.
#' If NULL, ignored. Default NULL.
#' @export
#' @autoglobal
trajectories_to_quantiles <- function(
    trajectories,
    quantiles = c(
      0.01,
      0.025,
      1:19 / 20,
      0.975,
      0.99
    ),
    timepoint_cols = "timepoint",
    value_col = "value",
    quantile_value_name = "quantile_value",
    quantile_level_name = "quantile_level",
    id_cols = NULL) {
  grouped_df <- trajectories |>
    dplyr::rename(value_col = !!value_col) |>
    dplyr::group_by(
      dplyr::across(tidyselect::all_of(c(timepoint_cols, id_cols)))
    )

  quant_df <- grouped_df |>
    filter(!is.na(value_col)) |>
    dplyr::reframe(
      !!quantile_value_name := stats::quantile(
        .data$value_col,
        probs = !!quantiles,
        na.rm = TRUE
      ),
      !!quantile_level_name := !!quantiles
    )
  return(quant_df)
}
