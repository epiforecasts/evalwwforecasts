# Targets script for analysing forecasts and scores
# This pipeline assumes that the `output/` folder contains:
# - overall_data_all_runs/scores.csv: a single file with all of the scores
# for all forecasts for the 3 models (wwinference with and without ww and
# baseline ARIMA)
# - individual_forecasts_all_runs/{forecast_date}/{location}/data: hospital
# admissions quantiles for wwinference with and without wastewater, R(t)
# estimates for the location with and without wastewater, and predicted
# quantiled wastewater concentrations


# The pipeline can be run using `tar_make()`

library(targets)
library(jsonlite)
library(httr)
library(tarchetypes)
library(wwinference)
library(dplyr)
library(ggplot2)
library(readr)
library(here)
library(purrr)
library(lubridate)
library(tidyr)
library(glue)
library(fs)
library(rlang)
library(scoringutils)
library(forecast)
library(future)
library(future.callr)

# load functions
functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)
rm("functions")

# load target modules
targets <- list.files(here("targets"), full.names = TRUE)
targets <- grep("*\\.R", targets, value = TRUE)
purrr::walk(targets, source)

tar_option_set(
  packages = c(
    "wwinference",
    "tibble",
    "dplyr",
    "ggplot2",
    "readr",
    "lubridate",
    "tidyr",
    "glue",
    "forecast",
    "jsonlite",
    "httr"
  ),
  workspace_on_error = TRUE,
  storage = "worker",
  retrieval = "worker",
  memory = "transient",
  garbage_collection = TRUE,
  format = "parquet", # default storage format
  error = "continue"
)

# Analysis config
analysis_config <- list(
  # Full set of dates and locations and models for which the model
  # was run for
  create_permutations_targets,
  # Set of dates and locations to focus on in example figures +
  # specifications of any post-processing model outputs
  analysis_config_targets
)

# Wastewater and state metadata
get_metadata <- list(
  tar_target(
    name = ww_metadata,
    command = read_csv(ww_metadata_fp)
  ),
  tar_target(
    name = state_pop_data,
    command = get_state_pop_data()
  )
)

# Secondary outputs
secondary_outputs <- list(
  # pairwise comparisons of scores between models
  pairwise_comparisons_targets,
  # GAM meta-model on scores ()
  run_gam_targets
  # compute coverage metrics (?)
)

# Figures
plot_targets <- list(
  analysis_EDA_plot_targets,
  multilocation_plot_targets,
  figure_targets
  # Fig 1: visual comparison for a single forecast date
  # Fig 2: visual comparison + scores across forecast dates
  # Fig 3: overall, by horizon, by location, by forecast date
  # by location and forecast date
  # Fig 4: Model-based evaluation results
)

list(
  analysis_config,
  get_metadata,
  secondary_outputs,
  plot_targets
)
