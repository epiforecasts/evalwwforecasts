# Targets script for generating forecasts and performing immediate
# post-processing (quantiling and scoring)

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

plan(multisession, workers = 2)

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
  error = "null"
)

## Set up the date:location:model:ww+/-:right-trunc+/- permutations
set_up <- list(
  create_permutations_targets
)


## Iterate over all permutations. For each:
# - extract the necessary data
# - pre-process the data based on the model's requirements
# - fit the model
# - extract posterior hospital admissions (calibration and forecast)
# - score the forecasts using CRPS and extract
# - quantile the calibration and forecasted admissions and extract
# - extract input data (hosp and/or ww)
# - extract model diagnostics

# Current set up: uses the `scenarios` tibble to do dynamic branching within
# each function via pattern = map(ind_data_created, scenarios)
load_data <- list(
  # Load data for each location/forecast date combination
  load_data_targets,
  load_baseline_data_targets
)
get_metadata <- list(
  get_metadata_targets
)
fit_models <- list(
  fit_model_targets,
  fit_baseline_model_targets
)

scoring <- list(
  scoring_targets
)

list(
  set_up,
  load_data,
  get_metadata,
  fit_models,
  scoring
)
