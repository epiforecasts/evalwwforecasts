#' Combining model draws with data for scoring
#'
#' @param fit_obj_wwinference wwinference_fit object
#' @param model_output Type of model output to extract, either "ww" or "hosp"
#' @param include_ww Whether model included wastewater data, "TRUE" or "FALSE"
#' @param model Model type, either "wwinference" or "baseline"
#' @param forecast_date Forecast date
#' @param location Location name
#' @param eval_data Dataframe of observed data to compare against model output
#' @return a dataframe containing model draws and observed data for scoring
#' @importFrom wwinference get_draws
#' @importFrom dplyr mutate rename left_join select ungroup
#' @importFrom lubridate ymd
#' @importFrom glue glue
#' @importFrom rlang arg_match .data abort
get_model_draws_w_data <- function(
    fit_obj_wwinference,
    model_output = c("ww", "hosp"),
    include_ww = TRUE,
    model = c("wwinference", "baseline"),
    forecast_date,
    location,
    eval_data) {
  model_output <- arg_match(model_output, values = c("ww", "hosp"))
  if (is.null(eval_data) || is.null(fit_obj_wwinference)) {
    return(NULL)
  }

  # Dataframe with columns
  if (model_output == "hosp") {
    new_hosp_draws <- wwinference::get_draws(
      fit_obj_wwinference,
      what = "predicted_counts"
    )$predicted_counts

    eval_data_min <- dplyr::select(eval_data, c(
      "date", "init_hosp_7d_count",
      "updated_hosp_7d_count"
    ))

    draws_w_data <- new_hosp_draws |>
      dplyr::mutate(
        name = "pred_hosp",
        include_ww = include_ww,
        model = !!model,
        forecast_date = lubridate::ymd(!!forecast_date),
        location = !!location,
        pred_value = pred_value
      )

    draws_w_data$value <- zoo::rollsum(draws_w_data$pred_value,
      k = 7,
      align = "right", na.pad = TRUE
    )

    draws_w_data <- draws_w_data |>
      dplyr::rename(
        calib_data = "observed_value",
        pop = "total_pop"
      ) |>
      dplyr::left_join(eval_data_min,
        by = "date"
      ) |>
      dplyr::rename(eval_data = "updated_hosp_7d_count") |>
      dplyr::ungroup()
  } else if
  (model_output == "ww") {
    new_ww_draws <- wwinference::get_draws(
      fit_obj_wwinference,
      what = "predicted_ww"
    )$predicted_ww

    ## Tested up to here ## - need to work out ww part

    eval_data_min <- eval_data |>
      dplyr::rename(
        below_lod_eval = "below_lod",
        log_lod_eval = "log_lod"
      ) |>
      dplyr::select(
        "date",
        "log_genome_copies_per_ml",
        "lab",
        "site",
        "exclude",
        "below_lod_eval",
        "log_lod_eval"
      ) |>
      unique()

    draws_w_data <- new_ww_draws |>
      dplyr::left_join(eval_data_min,
        by = c("date", "lab", "site")
      ) |>
      dplyr::rename(
        ww_pop = "subpop_pop",
        site_lab_name = "lab_site_name",
        flag_as_ww_outlier = "exclude",
        below_LOD = "below_lod"
      ) |>
      dplyr::mutate(
        name = "pred_ww",
        value = exp(.data$pred_value),
        calib_data = exp(.data$observed_value),
        eval_data = exp(.data$log_genome_copies_per_ml),
        lod_sewage = exp(.data$log_lod),
        lod_sewage_eval = exp(.data$log_lod_eval),
        forecast_date = lubridate::ymd(!!forecast_date),
        model = !!model,
        location = !!location
      ) |>
      dplyr::ungroup() |>
      # Replace values below LOD with LOD in observations
      dplyr::mutate(
        eval_data = ifelse(
          .data$below_lod_eval == 1,
          .data$lod_sewage_eval,
          .data$eval_data
        ),
        calib_data = ifelse(
          .data$below_LOD == 1,
          .data$lod_sewage,
          .data$eval_data
        )
      ) |>
      dplyr::select(
        "name",
        "lab_site_index",
        "value",
        "draw",
        "date",
        "site",
        "lab",
        "location",
        "ww_pop",
        "calib_data",
        "below_LOD",
        "lod_sewage",
        "below_lod_eval",
        "flag_as_ww_outlier",
        "eval_data",
        "forecast_date",
        "model_type",
        "scenario",
        "site_lab_name"
      )
  } else {
    abort(glue::glue("Unknown model_output {model_output}"), call = NULL)
  }

  return(draws_w_data)
}
