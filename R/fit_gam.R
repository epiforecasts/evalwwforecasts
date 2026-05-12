#' Prepare scores to model
#'
#' @param scores_long Long form data with scores by model and horizon
#' @param ww_metadata Metadata on wastewater at the location and forecast date
#'    level
#'
#' @returns Wide dataframe ready to be fit to model
#' @autoglobal
prep_scores_to_model <- function(scores_long,
                                 ww_metadata) {
  # Pivot scores from long to wide

  scores_joined <- scores_long |>
    mutate(model = glue::glue("{model}_{include_ww}")) |>
    pivot_wider(
      id_cols = c(
        "location", "forecast_date", "horizon",
        "hosp_data_real_time", "flag_missing_ww"
      ),
      names_from = "model",
      values_from = "wis",
      names_prefix = "wis_"
    ) |>
    left_join(ww_metadata, by = c(
      "location" = "location_name",
      "forecast_date"
    )) |>
    rename(
      wis_ww    = `wis_wwinference_TRUE`,
      wis_hosp  = `wis_wwinference_FALSE`,
      wis_arima = `wis_arima_baseline_FALSE`
    ) |>
    # GAM requires complete cases on all covariates
    filter(
      !is.na(wis_ww), !is.na(wis_hosp),
      !is.na(n_sites), !is.na(pop_coverage),
      !is.na(avg_sampling_freq), !is.na(avg_latency),
      !is.na(min_latency), !is.na(avg_data_variability)
    )
  return(scores_joined)
}

#' Fit the GAM model
#'
#' @param scores_to_model wide table of scores with wastewater metadata
#' @importFrom mgcv gam
#' @returns GAM object
fit_gam <- function(scores_to_model) {
  gam_fit <- gam(
    wis_ww ~ offset(log(wis_hosp)) +
      s(n_sites, k = 5) +
      s(pop_coverage, k = 5) +
      s(avg_sampling_freq, k = 5) +
      s(avg_latency, k = 5) +
      s(min_latency, k = 5) +
      s(avg_data_variability, k = 5),
    data = scores_to_model,
    family = Gamma(link = "log"), # or gaussian() for identity link
    method = "REML" # REML recommended for smoothing parameter selection
  )

  return(gam_fit)
}

#' Fit the GLM model
#'
#' @param scores_to_model wide table of scores with wastewater metadata
#' @importFrom stats glm
#' @returns GLM object
fit_glm <- function(scores_to_model) {
  glm_fit <- glm(
    wis_ww ~ offset(log(wis_hosp)) +
      n_sites +
      pop_coverage +
      avg_sampling_freq +
      avg_latency +
      min_latency +
      avg_data_variability,
    data = scores_to_model,
    family = Gamma(link = "log")
  )

  broom::tidy(glm_fit, conf.int = TRUE) |>
    mutate(across(c(estimate, conf.low, conf.high), exp, .names = "exp_{.col}")) |>
    select(term, exp_estimate, exp_conf.low, exp_conf.high, p.value) |>
    gt() |>
    fmt_number(decimals = 3) |>
    tab_header(
      title = "GLM coefficients (exponentiated)",
      subtitle = "exp(estimate) < 1: covariate associated with lower WIS_ww (better forecast)"
    )
  return(glm_fit)
}

#' Make a plot of the partial effects
#'
#' @param gam_fit GAM object
#' @importFrom gratia draw
#' @returns ggplot
partial_plot <- function(gam_fit) {
  s <- summary(gam_fit)


  partial_plots <- draw(gam_fit, residuals = TRUE, rug = TRUE) &
    theme_bw() &
    labs(y = "Partial effect on log(WIS_ww)")

  return(partial_plots)
}

plot_scores_fit_gam <- function(gam_fit, scores_to_model) {
  p <- scores_to_model |>
    mutate(fitted = fitted(gam_fit)) |>
    ggplot(aes(x = wis_hosp, y = wis_ww)) +
    geom_point(alpha = 0.3, size = 0.8) +
    geom_line(aes(y = fitted), colour = "firebrick", linewidth = 0.9) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
    scale_x_log10() +
    scale_y_log10() +
    theme_bw() +
    labs(
      x = "WIS (hosp-only model)",
      y = "WIS (ww+hosp model)",
      title = "Fitted vs observed WIS: wastewater vs hosp-only",
      subtitle = "Points below dashed line = wastewater model outperforms; red line = GAM fit"
    )

  return(p)
}
