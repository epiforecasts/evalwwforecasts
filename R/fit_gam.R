#' Prepare scores to model
#'
#' @param scores_long Long form data with scores by model and horizon
#' @param ww_metadata Metadata on wastewater at the location and forecast date
#'    level
#'
#' @returns Wide dataframe ready to be fit to model
#' @importFrom glue glue
#' @autoglobal
prep_scores_to_model <- function(scores_long,
                                 ww_metadata) {
  # Pivot scores from long to wide

  scores_joined <- scores_long |>
    mutate(model = glue("{model}_{include_ww}")) |>
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
      location = "location_name",
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
    ) |>
    mutate(horizon_weeks = ceiling((horizon) / 7))
  return(scores_joined)
}

#' Prepare scores for fitting a GAM
#'
#' Optionally z-scores the wastewater covariates, converts location to a
#' factor and forecast date to a numeric day index.
#'
#' @param scores_to_model wide table of scores with wastewater metadata
#' @param standardize logical, whether to z-score covariates before fitting
#'
#' @returns list with the data to fit and the scaling parameters (NULL if
#'   `standardize = FALSE`)
#' @importFrom dplyr mutate
#' @importFrom stats sd
#' @autoglobal
prep_gam_data <- function(scores_to_model, standardize = FALSE) {
  data_to_fit <- scores_to_model
  scaling_params <- NULL

  if (standardize) {
    covariates <- c(
      "n_sites", "pop_coverage", "avg_sampling_freq",
      "avg_latency", "min_latency", "avg_data_variability", "state_pop"
    )

    # Store means and SDs for later reference
    scaling_params <- data.frame(
      variable = covariates,
      mean = sapply(covariates, function(x) {
        return(mean(data_to_fit[[x]], na.rm = TRUE))
      }),
      sd = sapply(covariates, function(x) {
        return(sd(data_to_fit[[x]], na.rm = TRUE))
      })
    )

    for (covar in covariates) {
      data_to_fit[[covar]] <- scale(data_to_fit[[covar]])[, 1]
    }
  }

  data_to_fit <- data_to_fit |>
    mutate(
      location = factor(location),
      forecast_date_num = as.numeric(forecast_date - min(forecast_date))
    )

  return(list(data = data_to_fit, scaling_params = scaling_params))
}

#' Attach scaling and date information to a fitted GAM
#'
#' @param gam_fit fitted GAM
#' @param prepped output of `prep_gam_data()`
#'
#' @returns GAM object with `standardized`, `scaling_params` and
#'   `min_forecast_date` elements
add_gam_metadata <- function(gam_fit, prepped) {
  gam_fit$standardized <- !is.null(prepped$scaling_params)
  gam_fit$scaling_params <- prepped$scaling_params
  # Store reference date so forecast_date_num can be mapped back to dates
  gam_fit$min_forecast_date <- min(prepped$data$forecast_date)
  return(gam_fit)
}

#' Fit the GAM model
#'
#' Models WIS of the wastewater model with the hospital-only WIS as an offset,
#' so that exp(intercept + smooths) is the relative WIS (WIS_ww / WIS_hosp).
#'
#' Unweighted, the intercept is exactly the arithmetic mean of the
#' (covariate-adjusted) per-forecast ratios, which is biased in favour of the
#' hospital-only model (Bracher, Lerch, Pohle and Resin, 2026). Weighting each
#' forecast by `wis_hosp` instead makes the intercept a covariate-adjusted
#' ratio of mean scores, sum(WIS_ww) / sum(WIS_hosp).
#'
#' @param scores_to_model wide table of scores with wastewater metadata
#' @param standardize logical, whether to z-score covariates before fitting
#' @param weighted logical, whether to weight each forecast by `wis_hosp`
#' @importFrom mgcv gam
#' @returns GAM object (with scaling attributes if standardize = TRUE)
#' @autoglobal
fit_gam <- function(scores_to_model, standardize = FALSE, weighted = FALSE) {
  prepped <- prep_gam_data(scores_to_model, standardize)
  data_to_fit <- prepped$data
  # Weights are normalised to mean 1 so the dispersion stays on the same scale
  data_to_fit$gam_weight <- if (weighted) {
    data_to_fit$wis_hosp / mean(data_to_fit$wis_hosp)
  } else {
    1
  }

  gam_fit <- gam(
    wis_ww ~ offset(log(wis_hosp)) +
      s(horizon_weeks, k = 4) +
      s(location, bs = "re") +
      s(forecast_date_num, k = 20) +
      s(n_sites, k = 5) +
      s(pop_coverage, k = 5) +
      s(avg_sampling_freq, k = 5) +
      s(avg_latency, k = 5) +
      s(min_latency, k = 5) +
      s(avg_data_variability, k = 5),
    data = data_to_fit,
    weights = gam_weight,
    family = Gamma(link = "log"),
    method = "REML"
  )

  gam_fit <- add_gam_metadata(gam_fit, prepped)
  gam_fit$weighted <- weighted
  gam_fit$long_format <- FALSE

  return(gam_fit)
}

#' Fit the GLM model
#'
#' @param scores_to_model wide table of scores with wastewater metadata
#' @param standardize logical, whether to z-score covariates before fitting
#' @importFrom stats glm Gamma
#' @returns GLM object (with scaling attributes if standardize = TRUE)
#' @autoglobal
fit_glm <- function(scores_to_model, standardize = TRUE) {
  prepped <- prep_gam_data(scores_to_model, standardize)

  glm_fit <- glm(
    wis_ww ~ offset(log(wis_hosp)) +
      n_sites +
      pop_coverage +
      avg_sampling_freq +
      avg_latency +
      min_latency +
      avg_data_variability,
    data = prepped$data,
    family = Gamma(link = "log")
  )
  glm_fit$standardized <- standardize
  glm_fit$scaling_params <- prepped$scaling_params

  return(glm_fit)
}


#' Prepare the long-format scores for the GAMs
#'
#' Keeps the log-scale scores of the models in `models`, excludes
#' location-forecast dates where any model run was flagged for convergence
#' issues (if `exclude_flagged`), joins the wastewater metadata and keeps
#' only forecasts (location, forecast date and horizon) for which every
#' model, with and without wastewater, has a score, so that every model is
#' evaluated on the same set of forecasts.
#'
#' @param scores_long Long-format scores with one row per model, wastewater
#'   inclusion, location, forecast date and horizon, including `hosp_phase`
#' @param ww_metadata Metadata on wastewater at the location and forecast
#'   date level
#' @param models Named character vector of the models to include; names are
#'   the model types used in the GAMs. Default renewal (`wwinference`) and
#'   statistical (`ww_regression`). The first is the reference level.
#' @param exclude_flagged Logical, whether to exclude location-forecast dates
#'   where any model run failed the convergence checks (`any_flag`). Default
#'   `TRUE`.
#'
#' @returns Data.frame with one row per score, with `include_ww` an ordered
#'   factor, `model_type` and `hosp_phase` factors (stable as the reference
#'   phase) and `loc_date` a factor for each location-forecast date
#' @importFrom dplyr filter mutate left_join group_by ungroup n_distinct
#' @autoglobal
prep_scores_long_to_model <- function(scores_long,
                                      ww_metadata,
                                      models = c(
                                        renewal = "wwinference",
                                        statistical = "ww_regression"
                                      ),
                                      exclude_flagged = TRUE) {
  scores <- as.data.frame(scores_long)
  if ("scale" %in% names(scores)) {
    scores <- filter(scores, scale == "log")
  }
  if (isTRUE(exclude_flagged) && "any_flag" %in% names(scores)) {
    scores <- scores |>
      group_by(location, forecast_date) |>
      filter(!any(any_flag, na.rm = TRUE)) |>
      ungroup()
  }
  n_model_runs <- 2 * length(models)
  scores <- scores |>
    filter(model %in% models) |>
    left_join(ww_metadata, by = c(
      location = "location_name",
      "forecast_date"
    )) |>
    mutate(
      model_type = factor(
        names(models)[match(model, models)],
        levels = names(models)
      ),
      include_ww = ordered(include_ww, levels = c(FALSE, TRUE)),
      hosp_phase = factor(
        hosp_phase,
        levels = c("stable", "increasing", "decreasing")
      )
    ) |>
    # GAM requires complete cases on all covariates
    filter(
      !is.na(wis), !is.na(hosp_phase),
      !is.na(n_sites), !is.na(pop_coverage),
      !is.na(avg_sampling_freq), !is.na(min_latency),
      !is.na(avg_data_variability), !is.na(state_pop)
    ) |>
    # Same set of forecasts for every model
    group_by(location, forecast_date, horizon) |>
    filter(n_distinct(model, include_ww) == n_model_runs) |>
    ungroup() |>
    mutate(
      horizon_weeks = ceiling(horizon / 7),
      loc_date = factor(paste(location, forecast_date))
    )
  return(as.data.frame(scores))
}

#' Weighted effect coding for epidemic phase
#'
#' Sets the contrasts of `hosp_phase` so that each phase coefficient is the
#' deviation from the average over forecasts, weighted by the number of
#' forecasts in each phase. The main effect of a term interacting with
#' `hosp_phase` (e.g. `include_ww`) is then its average effect across
#' phases rather than its effect in the reference phase. The first level
#' (stable) has no coefficient: its deviation is minus the weighted sum of
#' the others' deviations divided by its count.
#'
#' @param data Data.frame with a factor `hosp_phase`
#'
#' @returns `data` with weighted effect contrasts on `hosp_phase`
#' @importFrom stats contrasts<-
set_phase_weighted_coding <- function(data) {
  n_phase <- table(data$hosp_phase)
  if (any(n_phase == 0)) {
    cli::cli_abort("Every epidemic phase must have at least one forecast.")
  }
  wec <- diag(length(n_phase))[, -1, drop = FALSE]
  wec[1, ] <- -n_phase[-1] / n_phase[1]
  dimnames(wec) <- list(names(n_phase), names(n_phase)[-1])
  contrasts(data$hosp_phase) <- wec
  return(data)
}

#' Fit the long-format GAM for one model type, with and without wastewater
#'
#' Stacks the WIS of the model with and without wastewater, for a single
#' model type (e.g. the renewal models), into one response. Every term has a
#' shared smooth, describing how forecast difficulty varies for both models,
#' and a difference smooth for the wastewater model (`by = include_ww`, an
#' ordered factor), describing how the effect of including wastewater
#' varies. exp(include_ww coefficient + difference smooths) is the relative
#' WIS. `hosp_phase` has weighted effect coding
#' (`set_phase_weighted_coding()`), so the include_ww coefficient is the
#' effect of wastewater averaged across phases (weighted by the number of
#' forecasts in each) and `include_ww:hosp_phase` the deviation from it in
#' the increasing and decreasing phases.
#'
#' Neither model's score appears in a denominator. In an intercept-only
#' version, exp(include_ww coefficient) is exactly the ratio of mean scores.
#' The pairing of the two scores for the same forecast is not modelled, so
#' standard errors on the difference smooths are likely conservative and
#' those on the shared smooths anti-conservative.
#'
#' @param scores_long_to_model Long-format scores from
#'   `prep_scores_long_to_model()`
#' @param model_type Character string, the model type to fit to. Default
#'   `"renewal"`.
#' @param standardize logical, whether to z-score covariates before fitting
#' @param k_date Integer, basis dimension of the forecast date smooths.
#'   Default `40`; use a smaller value for the sensitivity analysis with a
#'   less flexible forecast date smooth.
#' @importFrom mgcv bam
#' @importFrom stats contrasts<-
#' @returns GAM object (with scaling attributes if standardize = TRUE)
#' @autoglobal
fit_gam_long <- function(scores_long_to_model,
                         model_type = "renewal",
                         standardize = FALSE,
                         k_date = 40) {
  data_type <- scores_long_to_model[
    scores_long_to_model$model_type == model_type,
  ]
  prepped <- prep_gam_data(data_type, standardize)
  data_to_fit <- prepped$data
  # Ordered so `by = include_ww` gives difference smooths, but treatment
  # contrasts so the parametric include_ww term is the log relative WIS
  contrasts(data_to_fit$include_ww) <- "contr.treatment"
  # Phase effects as deviations from the average over forecasts, so the
  # include_ww term is the effect of wastewater averaged across phases
  data_to_fit <- set_phase_weighted_coding(data_to_fit)

  gam_fit <- bam(
    wis ~ include_ww * hosp_phase +
      s(horizon, k = 4) + s(horizon, k = 4, by = include_ww) +
      s(location, bs = "re") + s(location, bs = "re", by = include_ww) +
      s(forecast_date_num, k = k_date) +
      s(forecast_date_num, k = k_date, by = include_ww) +
      # One value per state, so keep this smooth simple
      s(state_pop, k = 3) + s(state_pop, k = 3, by = include_ww) +
      s(n_sites, k = 5) + s(n_sites, k = 5, by = include_ww) +
      s(pop_coverage, k = 5) + s(pop_coverage, k = 5, by = include_ww) +
      s(avg_sampling_freq, k = 5) +
      s(avg_sampling_freq, k = 5, by = include_ww) +
      s(min_latency, k = 5) + s(min_latency, k = 5, by = include_ww) +
      s(avg_data_variability, k = 5) +
      s(avg_data_variability, k = 5, by = include_ww),
    data = data_to_fit,
    family = Gamma(link = "log"),
    method = "fREML",
    # Not discretised, so the smooths are centred over the forecasts rather
    # than over the unique covariate values
    discrete = FALSE
  )

  gam_fit <- add_gam_metadata(gam_fit, prepped)
  gam_fit$weighted <- FALSE
  gam_fit$long_format <- TRUE
  gam_fit$model_type <- model_type
  gam_fit$phase_contrasts <- contrasts(data_to_fit$hosp_phase)

  return(gam_fit)
}

#' Fit the combined GAM across model types, with and without wastewater
#'
#' Fits all four models (each model type with and without wastewater) in one
#' GAM. `include_ww:model_type` is the difference in the effect of including
#' wastewater between model types, so that the relative WIS of including
#' wastewater is exp(include_ww) for the reference model type and
#' exp(include_ww + include_ww:model_type) for the other, averaged
#' across epidemic phases (weighted effect coding, as in `fit_gam_long()`). Each model has its own horizon smooth. The effects of the
#' wastewater surveillance characteristics, location, forecast date and
#' epidemic phase on the relative WIS are assumed to be shared across model
#' types. There is no random effect for each location-forecast date: with a
#' log link it would divide each forecast's weight by its difficulty, so
#' that hard forecasts count for less than in the relative WIS (ratio of
#' mean WIS). As in `fit_gam_long()`, the pairing of the scores for the same
#' forecast is not modelled, so standard errors are approximate.
#'
#' @inheritParams fit_gam_long
#' @importFrom mgcv bam
#' @importFrom stats contrasts<-
#' @returns GAM object
#' @autoglobal
fit_gam_combined <- function(scores_long_to_model,
                             standardize = FALSE,
                             k_date = 40) {
  prepped <- prep_gam_data(scores_long_to_model, standardize)
  data_to_fit <- prepped$data |>
    mutate(ww_model = interaction(model_type, include_ww, drop = TRUE))
  contrasts(data_to_fit$include_ww) <- "contr.treatment"
  data_to_fit <- set_phase_weighted_coding(data_to_fit)

  gam_fit <- bam(
    wis ~ include_ww * model_type + include_ww * hosp_phase +
      s(horizon, k = 4, by = ww_model) +
      s(location, bs = "re") + s(location, bs = "re", by = include_ww) +
      s(forecast_date_num, k = k_date) +
      s(forecast_date_num, k = k_date, by = include_ww) +
      s(state_pop, k = 3) + s(state_pop, k = 3, by = include_ww) +
      s(n_sites, k = 5) + s(n_sites, k = 5, by = include_ww) +
      s(pop_coverage, k = 5) + s(pop_coverage, k = 5, by = include_ww) +
      s(avg_sampling_freq, k = 5) +
      s(avg_sampling_freq, k = 5, by = include_ww) +
      s(min_latency, k = 5) + s(min_latency, k = 5, by = include_ww) +
      s(avg_data_variability, k = 5) +
      s(avg_data_variability, k = 5, by = include_ww),
    data = data_to_fit,
    family = Gamma(link = "log"),
    method = "fREML",
    # Not discretised, so the smooths are centred over the forecasts rather
    # than over the unique covariate values
    discrete = FALSE
  )

  gam_fit <- add_gam_metadata(gam_fit, prepped)
  gam_fit$weighted <- FALSE
  gam_fit$long_format <- TRUE
  gam_fit$model_type <- "combined"
  gam_fit$phase_contrasts <- contrasts(data_to_fit$hosp_phase)

  return(gam_fit)
}

#' Relative WIS of including wastewater for each model type
#'
#' From the combined GAM (`fit_gam_combined()`), computes the relative WIS
#' of including wastewater for each model type averaged across epidemic
#' phases, and the
#' ratio between model types (the `include_ww:model_type` interaction).
#'
#' @param gam_fit GAM from `fit_gam_combined()`
#' @param level Numeric, confidence level of the intervals. Default `0.95`.
#'
#' @returns Data.frame with `term`, `rel_wis`, `lower` and `upper`
#' @importFrom stats coef vcov qnorm
#' @autoglobal
get_combined_ww_effects <- function(gam_fit, level = 0.95) {
  z <- qnorm(1 - (1 - level) / 2)
  b <- coef(gam_fit)
  v <- vcov(gam_fit)
  types <- levels(gam_fit$model$model_type)
  interaction_name <- glue("include_wwTRUE:model_type{types[2]}")
  contrasts_list <- list(
    c("include_wwTRUE" = 1),
    c("include_wwTRUE" = 1, stats::setNames(1, interaction_name)),
    stats::setNames(1, interaction_name)
  )
  terms <- c(
    glue("Including wastewater, {types[1]} model"),
    glue("Including wastewater, {types[2]} model"),
    glue("Ratio, {types[2]} vs {types[1]}")
  )
  effects <- bind_rows(lapply(seq_along(contrasts_list), function(i) {
    w <- rep(0, length(b))
    names(w) <- names(b)
    w[names(contrasts_list[[i]])] <- contrasts_list[[i]]
    est <- sum(w * b)
    se <- sqrt(as.numeric(t(w) %*% v %*% w))
    return(data.frame(
      term = terms[i],
      rel_wis = exp(est),
      lower = exp(est - z * se),
      upper = exp(est + z * se)
    ))
  }))
  return(effects)
}

#' Plot the relative WIS of including wastewater for each model type
#'
#' @inheritParams get_combined_ww_effects
#' @importFrom ggplot2 ggplot aes geom_errorbar geom_point geom_hline
#'   scale_y_log10 labs theme_bw coord_flip
#' @returns ggplot object
#' @autoglobal
get_plot_combined_ww_effects <- function(gam_fit, level = 0.95) {
  effects <- get_combined_ww_effects(gam_fit, level)
  p <- ggplot(effects, aes(x = term, y = rel_wis)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
    geom_point(color = "seagreen", size = 5) +
    geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1) +
    scale_y_log10(breaks = rel_wis_breaks_fine, labels = rel_wis_labels_fine) +
    labs(
      y = "Relative WIS (adjusted)",
      x = NULL
    ) +
    theme_bw() +
    coord_flip()
  # Keep the GAM out of the plot environment
  rm(gam_fit)
  return(p)
}

#' Relative WIS of including wastewater by epidemic phase
#'
#' Computes the relative WIS of including wastewater in each hospital
#' admissions epidemic phase, as exp(include_ww + the phase deviation),
#' where the deviation is the phase's row of the `hosp_phase` contrasts
#' times the `include_ww:hosp_phase` coefficients, with intervals from the
#' coefficient covariance matrix. The smooths are at zero, i.e. averaged
#' over the forecasts they are centred over. For the combined GAM the
#' `include_ww:model_type` term is added for the non-reference model type.
#'
#' @param gam_fit GAM from `fit_gam_long()` or `fit_gam_combined()`
#' @param level Numeric, confidence level of the intervals. Default `0.95`.
#'
#' @returns Data.frame with `model_type`, `hosp_phase`, `gam`, `rel_wis`,
#'   `lower` and `upper`
#' @importFrom stats coef vcov qnorm
#' @autoglobal
get_ww_effect_by_phase <- function(gam_fit, level = 0.95) {
  z <- qnorm(1 - (1 - level) / 2)
  b <- coef(gam_fit)
  v <- vcov(gam_fit)
  phases <- levels(gam_fit$model$hosp_phase)
  combined <- "model_type" %in% names(gam_fit$model)
  types <- if (combined) {
    levels(gam_fit$model$model_type)
  } else {
    gam_fit$model_type
  }
  effects <- expand.grid(
    model_type = types, hosp_phase = phases,
    stringsAsFactors = FALSE
  )
  est <- lapply(seq_len(nrow(effects)), function(i) {
    w <- stats::setNames(rep(0, length(b)), names(b))
    w["include_wwTRUE"] <- 1
    contr <- gam_fit$phase_contrasts
    phase_terms <- paste0("include_wwTRUE:hosp_phase", colnames(contr))
    w[phase_terms] <- contr[effects$hosp_phase[i], ]
    type_term <- glue("include_wwTRUE:model_type{effects$model_type[i]}")
    if (combined && type_term %in% names(w)) w[type_term] <- 1
    return(c(sum(w * b), sqrt(as.numeric(t(w) %*% v %*% w))))
  })
  est <- do.call(rbind, est)
  effects$gam <- if (combined) "combined" else "separate"
  effects$rel_wis <- exp(est[, 1])
  effects$lower <- exp(est[, 1] - z * est[, 2])
  effects$upper <- exp(est[, 1] + z * est[, 2])
  return(effects)
}

#' Plot the relative WIS of including wastewater by epidemic phase
#'
#' One panel per model type, with the estimates from the separate GAM for
#' that model type and from the combined GAM.
#'
#' @param gam_fits List of GAMs from `fit_gam_long()` and
#'   `fit_gam_combined()`
#' @inheritParams get_ww_effect_by_phase
#' @importFrom ggplot2 ggplot aes geom_vline geom_linerange geom_point
#'   position_dodge scale_x_log10 scale_colour_manual facet_wrap labs
#'   theme_bw theme element_blank
#' @importFrom dplyr bind_rows mutate
#' @returns ggplot object
#' @autoglobal
get_plot_ww_effect_by_phase <- function(gam_fits, level = 0.95) {
  effects <- bind_rows(lapply(gam_fits, get_ww_effect_by_phase,
    level = level
  )) |>
    mutate(
      model_type = factor(
        model_type,
        levels = c("renewal", "statistical"),
        labels = c("Renewal model", "Simple statistical model")
      ),
      hosp_phase = factor(
        hosp_phase,
        levels = rev(c("increasing", "stable", "decreasing"))
      ),
      gam = factor(
        gam,
        levels = c("combined", "separate"),
        labels = c("Combined GAM", "Separate GAM")
      )
    )
  dodge <- position_dodge(width = 0.5)
  p <- ggplot(effects, aes(x = rel_wis, y = hosp_phase, colour = gam)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_linerange(aes(xmin = lower, xmax = upper),
      linewidth = 0.8, position = dodge
    ) +
    geom_point(size = 3, position = dodge) +
    scale_x_log10(breaks = rel_wis_breaks_fine, labels = rel_wis_labels_fine) +
    scale_colour_manual(
      values = c("Separate GAM" = "#2a78d6", "Combined GAM" = "#8a8a85"),
      breaks = c("Separate GAM", "Combined GAM"),
      name = NULL
    ) +
    facet_wrap(~model_type) +
    labs(
      x = "Relative WIS of including wastewater (adjusted)",
      y = "Hospital admissions epidemic phase",
      caption = glue(
        "Points and {level * 100}% intervals, with the other covariates ",
        "at their average effect. Below 1: wastewater improves the forecast."
      )
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
  # Keep the GAMs out of the plot environment
  rm(gam_fits)
  return(p)
}

#' Get the relative WIS of including wastewater in each location
#'
#' For each location, the GAM-estimated relative WIS of including
#' wastewater, exp of the mean over that location's forecasts of the
#' difference in the linear predictor with and without wastewater. This
#' includes every term that differs by wastewater inclusion (epidemic phase,
#' horizon, forecast date, wastewater surveillance characteristics and the
#' location random effect), evaluated at that location's own forecasts, so
#' it is the model-smoothed counterpart of the unadjusted ratio of mean WIS,
#' which is also returned.
#'
#' @param gam_fit GAM from `fit_gam_long()`
#' @param level Numeric, confidence level of the intervals. Default `0.95`.
#'
#' @returns Data.frame with `model_type`, `location`, `rel_wis`, `lower`,
#'   `upper` and `rel_wis_unadj` (mean WIS with wastewater over mean WIS
#'   without, across the location's forecasts)
#' @importFrom stats coef vcov qnorm predict
#' @importFrom dplyr group_by summarise left_join
#' @autoglobal
get_ww_effect_by_location <- function(gam_fit, level = 0.95) {
  z <- qnorm(1 - (1 - level) / 2)
  mf <- gam_fit$model
  nd_hosp <- mf[mf$include_ww == "FALSE", ]
  nd_ww <- nd_hosp
  nd_ww$include_ww[] <- "TRUE"
  diff_lp <- predict(gam_fit, newdata = nd_ww, type = "lpmatrix") -
    predict(gam_fit, newdata = nd_hosp, type = "lpmatrix")
  loc <- as.character(nd_hosp$location)
  avg_diff <- rowsum(diff_lp, loc) / as.vector(table(loc)[unique(sort(loc))])
  est <- as.vector(avg_diff %*% coef(gam_fit))
  se <- sqrt(rowSums((avg_diff %*% vcov(gam_fit)) * avg_diff))
  unadj <- mf |>
    group_by(location = as.character(location)) |>
    summarise(
      rel_wis_unadj = mean(wis[include_ww == "TRUE"]) /
        mean(wis[include_ww == "FALSE"])
    )
  effects <- data.frame(
    model_type = gam_fit$model_type,
    location = rownames(avg_diff),
    rel_wis = exp(est),
    lower = exp(est - z * se),
    upper = exp(est + z * se)
  ) |>
    left_join(unadj, by = "location")
  return(effects)
}

#' Plot the relative WIS of including wastewater by location
#'
#' One panel per model type, with the GAM-estimated relative WIS of
#' including wastewater in each location (from `get_ww_effect_by_location()`)
#' and the unadjusted ratio of mean WIS. Locations are ordered by the
#' estimate for the first model type, in the same order in every panel.
#'
#' @param gam_fits List of GAMs from `fit_gam_long()`, one per model type
#' @inheritParams get_ww_effect_by_location
#' @importFrom ggplot2 ggplot aes geom_vline geom_linerange geom_point
#'   scale_x_log10 scale_colour_manual scale_shape_manual facet_wrap labs
#'   theme_bw theme element_blank
#' @importFrom dplyr bind_rows mutate filter arrange pull
#' @returns ggplot object
#' @autoglobal
get_plot_ww_effect_by_location <- function(gam_fits, level = 0.95) {
  effects <- bind_rows(lapply(gam_fits, get_ww_effect_by_location,
    level = level
  ))
  loc_order <- effects |>
    filter(model_type == effects$model_type[1]) |>
    arrange(rel_wis) |>
    pull(location)
  effects <- effects |>
    mutate(
      model_type = factor(
        model_type,
        levels = c("renewal", "statistical"),
        labels = c("Renewal model", "Simple statistical model")
      ),
      location = factor(location, levels = rev(loc_order))
    )
  p <- ggplot(effects, aes(y = location)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_linerange(aes(xmin = lower, xmax = upper),
      linewidth = 0.8, colour = "#2a78d6"
    ) +
    geom_point(
      aes(x = rel_wis, colour = "GAM estimate", shape = "GAM estimate"),
      size = 3
    ) +
    geom_point(
      aes(
        x = rel_wis_unadj, colour = "Unadjusted", shape = "Unadjusted"
      ),
      size = 2.5
    ) +
    scale_x_log10(breaks = rel_wis_breaks, labels = rel_wis_labels) +
    scale_colour_manual(
      values = c("GAM estimate" = "#2a78d6", "Unadjusted" = "#8a8a85"),
      name = NULL
    ) +
    scale_shape_manual(
      values = c("GAM estimate" = 16, "Unadjusted" = 4),
      name = NULL
    ) +
    facet_wrap(~model_type) +
    labs(
      x = "Relative WIS of including wastewater",
      y = NULL,
      caption = glue(
        "GAM estimate: mean over the location's forecasts, with ",
        "{level * 100}% interval. Unadjusted: ratio of mean WIS. ",
        "Below 1: wastewater improves the forecast."
      )
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
  # Keep the GAMs out of the plot environment
  rm(gam_fits)
  return(p)
}

#' Get plot of location specific multiplicative effect
#'
#' @param gam_fit long-format GAM fit from `fit_gam_long()`
#' @importFrom mgcv gam.check
#' @importFrom ggplot2 geom_errorbar coord_flip scale_x_discrete
#' @returns ggplot object
#' @autoglobal
get_plot_effect_by_location <- function(gam_fit) {
  # Tables and plots that I will export to separate figures
  par(mfrow = c(2, 2))
  gam.check(gam_fit)
  par(mfrow = c(1, 1))

  estimates <- smooth_estimates(gam_fit)

  make_ci_table <- function(smooth_name, group_var) {
    cis <- estimates |>
      filter(.smooth == smooth_name) |>
      select(.smooth, {{ group_var }}, .estimate, .se) |>
      rename(est = .estimate, se = .se) |>
      mutate(
        ci_lower = est - 1.96 * se,
        ci_upper = est + 1.96 * se,
        effect = exp(est),
        est_lower = exp(ci_lower),
        est_upper = exp(ci_upper),
        excludes_zero = ci_lower > 0 | ci_upper < 0
      ) |>
      arrange(effect)
    return(cis)
  }

  location_ci <- make_ci_table("s(location):include_wwTRUE", location)

  knitr::kable(select(location_ci, -.smooth))

  p <- ggplot(
    location_ci,
    aes(x = reorder(location, -effect), y = effect)
  ) +
    geom_errorbar(aes(ymin = est_lower, ymax = est_upper), width = 0.2) +
    geom_point(color = "seagreen", size = 5) +
    geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1) +
    scale_y_log10(breaks = rel_wis_breaks, labels = rel_wis_labels) +
    labs(
      y = "Multiplicative effect on WIS of including wastewater",
      x = "Location"
    ) +
    theme_bw() +
    coord_flip() +
    theme(
      text            = element_text(size = 27),
      axis.text.x     = element_text(size = 24),
      axis.text.y     = element_text(size = 24),
      axis.title      = element_text(size = 27),
      legend.text     = element_text(size = 24),
      legend.title    = element_text(size = 25.5),
      strip.text      = element_text(size = 25.5)
    ) +
    scale_x_discrete(labels = function(x) gsub("-", "-\n", x)) # nolint

  return(p)
}

#' Get plot of the overall multiplicative effect of including wastewater
#'
#' Plots exp(beta_ww), the exponentiated `include_ww` coefficient from the
#' long-format GAM, with a Wald confidence interval. Because the difference
#' smooths are centred, this is the relative WIS (WIS_ww / WIS_hosp) with all
#' difference smooths at zero.
#'
#' @param gam_fit long-format GAM fit from `fit_gam_long()`
#' @param level confidence level for the interval
#'
#' @returns ggplot object
#' @importFrom stats coef vcov qnorm
#' @autoglobal
get_plot_effect_ww <- function(gam_fit, level = 0.95) {
  coef_name <- "include_wwTRUE"
  est <- coef(gam_fit)[[coef_name]]
  se <- sqrt(vcov(gam_fit)[coef_name, coef_name])
  z <- qnorm(1 - (1 - level) / 2)
  # nolint start
  ww_effect <- data.frame(
    term = "Include wastewater",
    effect = exp(est),
    est_lower = exp(est - z * se),
    est_upper = exp(est + z * se)
  )
  # nolint end

  p <- ggplot(ww_effect, aes(x = term, y = effect)) +
    geom_errorbar(aes(ymin = est_lower, ymax = est_upper), width = 0.1) +
    geom_point(color = "seagreen", size = 5) +
    geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1) +
    scale_y_log10(breaks = rel_wis_breaks, labels = rel_wis_labels) +
    labs(
      y = "Multiplicative effect on WIS of including wastewater",
      x = NULL
    ) +
    theme_bw() +
    coord_flip() +
    theme(
      text = element_text(size = 27),
      axis.text.x = element_text(size = 24),
      axis.text.y = element_text(size = 24),
      axis.title = element_text(size = 27)
    )

  return(p)
}

#' Get plot of the multiplicative effect of wastewater characteristics
#'
#' Plots exp(difference smooth) for each wastewater characteristic from the
#' long-format GAM, i.e. how the multiplicative effect of including
#' wastewater on WIS changes across values of that characteristic. Smooths
#' are centred, so the curves are relative to the overall effect,
#' exp(beta_ww), and exclude its uncertainty.
#'
#' @param gam_fit long-format GAM fit from `fit_gam_long()`
#' @param vars character vector of wastewater characteristics to plot
#' @param level confidence level for the interval
#' @param n_grid number of points to evaluate each smooth at
#'
#' @returns ggplot object
#' @importFrom gratia smooth_estimates
#' @importFrom dplyr bind_rows filter mutate
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_hline
#'   facet_wrap scale_y_log10 labs theme_bw theme element_text
#' @importFrom stats qnorm
#' @autoglobal
get_plot_ww_chars <- function(gam_fit,
                              vars = c(
                                "n_sites", "pop_coverage", "avg_sampling_freq",
                                "avg_latency", "min_latency",
                                "avg_data_variability"
                              ),
                              level = 0.95,
                              n_grid = 100) {
  z <- qnorm(1 - (1 - level) / 2)

  smooth_ci <- bind_rows(lapply(vars, function(v) {
    est <- smooth_estimates(
      gam_fit,
      select = glue("s({v}):include_wwTRUE"),
      n = n_grid
    )
    return(data.frame(
      variable = v,
      x = est[[v]],
      effect = exp(est$.estimate),
      est_lower = exp(est$.estimate - z * est$.se),
      est_upper = exp(est$.estimate + z * est$.se)
    ))
  }))

  p <- ggplot(smooth_ci, aes(x = x, y = effect)) +
    geom_ribbon(aes(ymin = est_lower, ymax = est_upper),
      fill = "seagreen", alpha = 0.3
    ) +
    geom_line(color = "seagreen", linewidth = 1.5) +
    geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1) +
    scale_y_log10(breaks = rel_wis_breaks, labels = rel_wis_labels) +
    facet_wrap(~variable, scales = "free_x") +
    labs(
      y = "Multiplicative effect on WIS of including wastewater",
      x = ""
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 27),
      axis.text.x = element_text(size = 24),
      axis.text.y = element_text(size = 24),
      axis.title = element_text(size = 27),
      strip.text = element_text(size = 25.5)
    )

  return(p)
}

# Breaks for log-scale axes of relative WIS, in reciprocal pairs (r, 1 / r)
# so ticks are symmetric about 1
rel_wis_breaks <- c(1 / 3, 0.5, 2 / 3, 0.8, 0.9, 1, 1 / 0.9, 1.25, 1.5, 2, 3)
rel_wis_labels <- c(
  "0.33", "0.5", "0.67", "0.8", "0.9", "1", "1.11", "1.25", "1.5", "2", "3"
)
# Finer breaks for effects close to 1
rel_wis_breaks_fine <- c(0.9, 0.95, 0.97, 1, 1 / 0.97, 1 / 0.95, 1 / 0.9)
rel_wis_labels_fine <- c("0.9", "0.95", "0.97", "1", "1.03", "1.05", "1.11")

#' Convert a GAM covariate back to its original scale
#'
#' @param gam_fit GAM object returned by `fit_gam()` or `fit_gam_long()`
#' @param var Name of the covariate
#' @param x Values of the covariate on the scale used in the fit
#'
#' @returns Values on the original (unstandardized) scale
#' @autoglobal
gam_var_to_original_scale <- function(gam_fit, var, x) {
  if (isTRUE(gam_fit$standardized) &&
    var %in% gam_fit$scaling_params$variable) {
    params <- gam_fit$scaling_params[gam_fit$scaling_params$variable == var, ]
    x <- x * params$sd + params$mean
  }
  return(x)
}

#' Build a placeholder row of covariates for predicting from a GAM
#'
#' @param gam_fit GAM object returned by `fit_gam()` or `fit_gam_long()`
#' @param n Number of rows
#'
#' @returns data.frame with every covariate at its median (or first level)
#' @importFrom stats median
#' @autoglobal
get_gam_newdata <- function(gam_fit, n) {
  vars <- unique(unlist(lapply(gam_fit$smooth, `[[`, "term")))
  newdata <- lapply(vars, function(v) {
    x <- gam_fit$model[[v]]
    if (is.factor(x)) {
      return(factor(levels(x)[1], levels = levels(x)))
    }
    return(median(x))
  })
  names(newdata) <- vars
  # Parametric factors (e.g. epidemic phase, model type) at their first
  # level; their interactions with include_ww are dropped or kept in
  # get_rel_wis_lpmatrix()
  for (v in setdiff(names(gam_fit$model), c(vars, "wis", "include_ww"))) {
    x <- gam_fit$model[[v]]
    if (is.factor(x)) {
      newdata[[v]] <- factor(levels(x)[1], levels = levels(x))
    }
  }
  newdata <- as.data.frame(newdata)[rep(1, n), , drop = FALSE]
  newdata$wis_hosp <- 1
  return(newdata)
}

#' Linear predictor matrix for log relative WIS across values of one term
#'
#' Returns a matrix L such that `L %*% coef(gam_fit)` is the log relative WIS
#' (log WIS_ww / WIS_hosp) at each value of `var`, with all other terms at
#' their average (zero) effect:
#' - offset model (`fit_gam()`): intercept + s(var)
#' - long-format model (`fit_gam_long()`): include_ww coefficient + the
#'   wastewater difference smooth of `var`
#'
#' @param gam_fit GAM object returned by `fit_gam()` or `fit_gam_long()`
#' @param var Name of the variable to vary
#' @param values Values of `var` (on the scale used in the fit)
#'
#' @returns matrix with one row per value and one column per coefficient
#' @importFrom stats predict
#' @autoglobal
get_rel_wis_lpmatrix <- function(gam_fit, var, values) {
  newdata <- get_gam_newdata(gam_fit, length(values))
  newdata[[var]] <- values

  if (isTRUE(gam_fit$long_format)) {
    ww_levels <- levels(gam_fit$model$include_ww)
    newdata_ww <- newdata
    newdata$include_ww <- factor(
      ww_levels[1],
      levels = ww_levels, ordered = TRUE
    )
    newdata_ww$include_ww <- factor(
      ww_levels[2],
      levels = ww_levels, ordered = TRUE
    )
    # Shared terms cancel in the difference, leaving the wastewater effect
    lp <- predict(gam_fit, newdata = newdata_ww, type = "lpmatrix") -
      predict(gam_fit, newdata = newdata, type = "lpmatrix")
    keep_smooth <- function(sm) {
      return(sm$term == var && sm$by == "include_ww")
    }
    # Phase deviations are dropped, so the effect is averaged across phases
    keep_param <- grep("^include_ww", colnames(lp), value = TRUE)
    keep_param <- keep_param[!grepl("hosp_phase", keep_param)]
  } else {
    lp <- predict(gam_fit, newdata = newdata, type = "lpmatrix")
    keep_smooth <- function(sm) {
      return(sm$term == var)
    }
    keep_param <- "(Intercept)"
  }

  # Zero out every coefficient except the parametric ww term and var's smooth
  keep <- colnames(lp) %in% keep_param
  for (sm in gam_fit$smooth) {
    if (keep_smooth(sm)) keep[sm$first.para:sm$last.para] <- TRUE
  }
  lp[, !keep] <- 0

  return(lp)
}

#' Average relative WIS implied by a GAM
#'
#' @param gam_fit GAM object returned by `fit_gam()` or `fit_gam_long()`
#'
#' @returns exp of the intercept (offset model) or of the include_ww
#'   coefficient (long-format model)
#' @importFrom stats coef
#' @autoglobal
get_gam_avg_rel_wis <- function(gam_fit) {
  term <- if (isTRUE(gam_fit$long_format)) {
    "include_wwTRUE"
  } else {
    "(Intercept)"
  }
  return(exp(coef(gam_fit)[[term]]))
}


#' Estimate the effect of each covariate on relative WIS
#'
#' For each covariate, computes the multiplicative change in relative WIS
#' when moving from its lower to its upper quantile, i.e.
#' exp(s(x_upper) - s(x_lower)), with confidence intervals from the
#' linear predictor matrix. This puts the smooth effects on a common scale
#' comparable to exponentiated GLM coefficients.
#'
#' @param gam_fit GAM object returned by `fit_gam()` or `fit_gam_long()`
#' @param vars Continuous variables to summarise
#' @param lower_q,upper_q Quantiles of each covariate to contrast
#' @param level Width of the confidence interval
#'
#' @returns data.frame with one row per covariate
#' @importFrom stats predict quantile qnorm coef vcov
#' @importFrom dplyr bind_rows
#' @autoglobal
get_gam_effect_sizes <- function(
  gam_fit,
  vars = c(
    "horizon_weeks", "n_sites", "pop_coverage", "avg_sampling_freq",
    "avg_latency", "min_latency", "avg_data_variability"
  ),
  lower_q = 0.1,
  upper_q = 0.9,
  level = 0.95
) {
  z <- qnorm(1 - (1 - level) / 2)

  effect_sizes <- bind_rows(lapply(vars, function(v) {
    quantiles <- quantile(
      gam_fit$model[[v]], c(lower_q, upper_q),
      names = FALSE
    )
    # Contrast of the two rows: the average relative WIS cancels
    lp <- get_rel_wis_lpmatrix(gam_fit, v, quantiles)
    contrast <- lp[2, ] - lp[1, ]
    est <- sum(contrast * coef(gam_fit))
    se <- sqrt(as.numeric(t(contrast) %*% vcov(gam_fit) %*% contrast))
    q_orig <- gam_var_to_original_scale(gam_fit, v, quantiles)
    return(data.frame(
      variable = v,
      from = q_orig[1],
      to = q_orig[2],
      ratio = exp(est),
      lower = exp(est - z * se),
      upper = exp(est + z * se)
    ))
  }))

  return(effect_sizes)
}

#' Forest plot of covariate effects on relative WIS
#'
#' @inheritParams get_gam_effect_sizes
#'
#' @returns ggplot
#' @importFrom ggplot2 ggplot aes geom_pointrange geom_vline scale_x_log10
#'   theme_bw labs
#' @importFrom glue glue
#' @importFrom stats reorder
#' @autoglobal
plot_gam_effect_sizes <- function(gam_fit, lower_q = 0.1, upper_q = 0.9) {
  effect_sizes <- get_gam_effect_sizes(
    gam_fit,
    lower_q = lower_q, upper_q = upper_q
  )
  effect_sizes$label <- glue(
    "{effect_sizes$variable}\n",
    "({signif(effect_sizes$from, 3)} \u2192 {signif(effect_sizes$to, 3)})"
  )

  p <- ggplot(effect_sizes, aes(
    x = ratio, y = reorder(label, ratio),
    xmin = lower, xmax = upper
  )) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_pointrange() +
    scale_x_log10(breaks = rel_wis_breaks, labels = rel_wis_labels) +
    theme_bw() +
    labs(
      x = "Multiplicative change in relative WIS",
      y = NULL,
      title = "Effect of wastewater characteristics on relative WIS",
      subtitle = glue(
        "Change from {lower_q * 100}th to {upper_q * 100}th percentile; ",
        "< 1: wastewater benefit increases"
      )
    )

  return(p)
}


#' Table of exponentiated GLM coefficients
#'
#' @param glm_fit GLM object returned by `fit_glm()`
#' @importFrom broom tidy
#' @importFrom dplyr mutate across select
#' @importFrom gt gt fmt_number tab_header
#' @returns gt table
#' @autoglobal
get_glm_coef_table <- function(glm_fit) {
  coef_table <- tidy(glm_fit, conf.int = TRUE) |>
    mutate(across(
      c(estimate, conf.low, conf.high), exp,
      .names = "exp_{.col}"
    )) |>
    select(term, exp_estimate, exp_conf.low, exp_conf.high, p.value) |>
    gt() |>
    fmt_number(decimals = 3) |>
    tab_header(
      title = "GLM coefficients (exponentiated)",
      subtitle = paste(
        "exp(estimate) < 1: covariate associated with lower WIS_ww",
        "(better forecast)"
      )
    )

  return(coef_table)
}

#' Get residual diagnostic plots for a GAM
#'
#' Combines the standard `gratia::appraise()` panels (QQ plot, residuals vs
#' linear predictor, histogram, observed vs fitted) with deviance residuals
#' against forecast horizon and forecast date. For the long-format model these
#' are split by model (hospital only vs wastewater), and a final panel plots
#' the residuals of the two models for the same forecast against each other.
#' The long-format model treats these as independent, so a strong
#' correlation means its standard errors are mis-stated.
#'
#' @param gam_fit GAM fit from `fit_gam()` or `fit_gam_long()`
#'
#' @returns patchwork object
#' @importFrom gratia appraise
#' @importFrom mgcv k.check
#' @importFrom stats residuals fitted cor
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth geom_hline
#'   geom_abline labs theme_bw
#' @importFrom patchwork wrap_plots
#' @autoglobal
get_plot_gam_diagnostics <- function(gam_fit) {
  # Basis dimension check: k-index well below 1 with a small p-value suggests
  # k is too low for that smooth
  print(k.check(gam_fit))

  long_format <- isTRUE(gam_fit$long_format)
  # Residuals can only be paired by forecast for a single model type
  paired_resid <- long_format && !"model_type" %in% names(gam_fit$model)
  horizon_var <- intersect(c("horizon", "horizon_weeks"), names(gam_fit$model))
  resid_data <- gam_fit$model |>
    mutate(
      resid = residuals(gam_fit, type = "deviance"),
      fitted = fitted(gam_fit),
      horizon = .data[[horizon_var[1]]],
      model = if (long_format) {
        ww_label <- ifelse(include_ww == "TRUE", "wastewater", "hospital only")
        if (paired_resid) ww_label else paste(model_type, ww_label)
      } else {
        "wastewater vs hospital only"
      }
    )

  p_appraise <- appraise(gam_fit, point_alpha = 0.1)

  p_horizon <- ggplot(resid_data, aes(x = horizon, y = resid, colour = model)) +
    geom_point(alpha = 0.05) +
    geom_smooth(se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Horizon", y = "Deviance residual", colour = NULL) +
    theme_bw()

  p_date <- ggplot(
    resid_data,
    aes(x = forecast_date_num, y = resid, colour = model)
  ) +
    geom_point(alpha = 0.05) +
    geom_smooth(se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Days since first forecast date", y = "Deviance residual",
      colour = NULL
    ) +
    theme_bw()

  plots <- list(p_horizon, p_date)

  if (paired_resid) {
    paired <- resid_data |>
      select(location, forecast_date_num, horizon, model, resid) |>
      pivot_wider(names_from = model, values_from = resid)
    resid_cor <- cor(
      paired[["hospital only"]], paired[["wastewater"]],
      use = "complete.obs"
    )
    p_paired <- ggplot(
      paired,
      aes(x = .data[["hospital only"]], y = .data[["wastewater"]])
    ) +
      geom_point(alpha = 0.05) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      labs(
        x = "Residual, hospital only", y = "Residual, wastewater",
        title = sprintf("Paired residual correlation = %.2f", resid_cor)
      ) +
      theme_bw()
    plots <- c(plots, list(p_paired))
  }

  p <- wrap_plots(p_appraise, wrap_plots(plots, nrow = 1), ncol = 1)

  return(p)
}

#' Plot fitted against observed WIS
#'
#' @param gam_fit GAM object returned by `fit_gam()`
#' @param scores_to_model wide table of scores the GAM was fit to
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_abline
#'   scale_x_log10 scale_y_log10 theme_bw labs
#' @importFrom dplyr mutate
#' @importFrom stats fitted
#' @returns ggplot
#' @autoglobal
plot_scores_fit_gam <- function(gam_fit, scores_to_model) {
  p <- scores_to_model |>
    mutate(fitted = fitted(gam_fit)) |>
    ggplot(aes(x = wis_hosp, y = wis_ww)) +
    geom_point(alpha = 0.3, size = 0.8) +
    geom_line(aes(y = fitted), colour = "firebrick", linewidth = 0.9) +
    geom_abline(
      slope = 1, intercept = 0,
      linetype = "dashed", colour = "grey40"
    ) +
    scale_x_log10() +
    scale_y_log10() +
    theme_bw() +
    labs(
      x = "WIS (hosp-only model)",
      y = "WIS (ww+hosp model)",
      title = "Fitted vs observed WIS: wastewater vs hosp-only",
      subtitle = paste(
        "Points below dashed line = wastewater model outperforms;",
        "red line = GAM fit"
      )
    )

  return(p)
}
