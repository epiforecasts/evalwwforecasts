get_model_draws_w_data <- function(
    fit_obj_wwinference,
    model_output = c("ww", "hosp"),
    include_ww = c("TRUE", "FALSE"),
    model = c("wwinference", "baseline"),
    forecast_date,
    location,
    eval_data) {
  model_output <- arg_match(model_output, values = c("ww", "hosp"))
  if (is.null(eval_data) || is.null(fit_obj_wwinference)) {
    return(NULL)
  }

  # Not needed because currently have eval_data specific to location
  #  eval_data <- eval_data |>
  #    dplyr::filter(location == !!location)
  #  stopifnot(
  #    "More than one location in eval data that is getting joined" = eval_data |>
  #      dplyr::pull(location) |>
  #      unique() |>
  #      length() ==
  #      1
  #  )

  # Dataframe with columns
  if (model_output == "hosp") {
    new_hosp_draws <- wwinference::get_draws(
      fit_obj_wwinference,
      what = "predicted_counts"
    )$predicted_counts

    draws_w_data <- new_hosp_draws |>
      dplyr::mutate(
        "name" = "pred_hosp",
        "include_ww" = !!include_ww,
        "model" = !!model,
        "forecast_date" = lubridate::ymd(!!forecast_date),
        "location" = !!location,
      ) |>
      dplyr::rename(
        "value" = "pred_value",
        "calib_data" = "observed_value",
        "pop" = "total_pop"
      ) |>
      dplyr::left_join(
        eval_data |>
          dplyr::select("date", "daily_hosp_admits"),
        by = c("date")
      ) |>
      dplyr::rename("eval_data" = "daily_hosp_admits") |>
      dplyr::ungroup()
  } else if (model_output == "ww") {
    new_ww_draws <- wwinference::get_draws(
      fit_obj_wwinference,
      what = "predicted_ww"
    )$predicted_ww

    ## Tested up to here ## - need to work out ww part

    draws_w_data <- new_ww_draws |>
      dplyr::left_join(
        eval_data |>
          dplyr::rename(
            "below_lod_eval" = "below_lod",
            "log_lod_eval" = "log_lod"
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
          unique(),
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
        model_type = !!model_type,
        scenario = !!scenario,
        location = !!location
      ) |>
      dplyr::ungroup() |>
      # Replace values below LOD with LOD in observations
      dplyr::mutate(
        "eval_data" = ifelse(
          .data$below_lod_eval == 1,
          .data$lod_sewage_eval,
          .data$eval_data
        ),
        "calib_data" = ifelse(
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
    stop(glue::glue("Unknown model_output {model_output}"))
  }

  return(draws_w_data)
}
