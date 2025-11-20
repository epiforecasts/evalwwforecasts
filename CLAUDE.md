# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R package (`evalwwforecasts`) that evaluates wastewater-informed forecasts of hospital admissions. The project uses a targets-based workflow to generate forecasts and compare models that incorporate wastewater data vs. baseline models that do not.

## Development Commands

### Package Management

```r
# Restore package dependencies (uses renv)
renv::restore()

# Install the package locally for development
devtools::install()

# Load all package functions
devtools::load_all()
```

### Running the Pipeline

```r
# Run the full targets pipeline
targets::tar_make()

# Visualize the pipeline dependency graph
targets::tar_visnetwork()

# Check which targets are outdated
targets::tar_outdated()

# Load a specific target
targets::tar_load(target_name)

# Read a specific target
targets::tar_read(target_name)
```

### Testing and Quality Control

```r
# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-quiet.R")

# Check the package (R CMD check)
devtools::check()

# Run linter
lintr::lint_package()
```

### Pre-commit Hooks

The project uses pre-commit hooks for code quality. Key checks include:
- `style-files`: Auto-format code with styler (tidyverse style)
- `lintr`: Check code quality
- `parsable-R`: Ensure R code is parsable
- `no-browser-statement`: Prevent debug statements

To run manually:
```bash
pre-commit run --all-files
```

## Architecture

### Targets Pipeline Structure

The pipeline is organized in `_targets.R` which sources modular target files from the `targets/` directory:

1. **Setup (`create_permutations_targets.R`)**: Creates all combinations of forecast dates, locations (German states), models, wastewater inclusion (yes/no), and data types (real-time vs final). Each combination is a "scenario" that will be run through the pipeline.

2. **Data Loading** (`load_data_targets.R`, `load_baseline_data_targets.R`): Loads hospital admissions and wastewater data for each location/forecast date combination.

3. **Metadata** (`get_metadata_targets.R`): Calculates metadata about wastewater data quality and availability.

4. **Model Fitting** (`fit_model_targets.R`, `fit_baseline_model_targets.R`):
   - Main models use `wwinference` (a Bayesian inference model from CDCgov)
   - Baseline models use ARIMA
   - Uses dynamic branching: `pattern = map(...)` to run each scenario in parallel

5. **Scoring** (`scoring_targets.R`): Evaluates forecast quality using scoring metrics (CRPS, WIS) from the `scoringutils` package.

### Key R Functions

**Data Processing:**
- `R/get_hosp_data.R`: Fetches and formats hospital admission data
- `R/get_ww_data.R`: Fetches and formats wastewater concentration data
- `R/get_metadata.R`: Calculates wastewater metadata (sampling frequency, lab changes, etc.)

**Model Fitting:**
- `R/fit_wwinference_wrapper.R`: Wrapper that fits the wwinference model, generates forecasts, saves plots and quantiles
- `R/fit_arima.R`: Fits ARIMA baseline model

**Utilities:**
- `R/score.R`: Functions for scoring forecast performance
- `R/convert_to_su_object.R`: Convert data to scoringutils format
- `R/get_model_draws_w_data.R`: Extract posterior draws with evaluation data
- `R/EDA_plots.R`: Plotting functions for model comparisons
- `R/utils.R`: General utility functions

### Data Flow

1. Raw data is stored in `input/data/`
2. The pipeline creates "scenarios" (all combinations of: 16 locations × ~52 forecast dates × 2 models × 2 ww inclusion states)
3. For each scenario:
   - Load and preprocess data
   - Fit model (either wwinference or ARIMA)
   - Generate forecasts
   - Save outputs to `output/individual_forecasts/{forecast_date}/{location}/`
   - Calculate scores
4. Aggregate results are saved to `output/overall_data/` and `output/overall_figs/`

### Parallel Processing

The pipeline uses `future` for parallel processing:
- Workers set to `floor(availableCores() / 4)` in `_targets.R`
- Targets are configured with `storage = "worker"` and `retrieval = "worker"`
- Model fitting uses `deployment = "worker"`

### wwinference Integration

The main forecasting model is `wwinference`, a CDC package for wastewater-informed forecasting:
- Compiled Stan model is cached in `compiled_models/`
- Model specification includes generation interval, infection-to-hospitalization delay, and infection feedback
- Default: 500 sampling iterations, 250 warmup iterations, 4 parallel chains

### Output Structure

Each model run saves to `output/individual_forecasts/{forecast_date}/{location}/`:
- `figs/`: PNG plots of hospital and wastewater forecasts
- `data/`: CSV and Parquet files with quantiles, draws, and R(t) estimates

## Important Implementation Details

### Global Variables

The package uses `roxyglobals` to manage global variables. All global variables used in functions (like column names accessed with `dplyr`) are tracked in `R/globals.R`. This file is auto-generated by roxyglobals.

### Quantiles

Standard quantiles used throughout: `c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)`

### Missing Wastewater Data

If a location/date is supposed to include wastewater but none is available, the model automatically falls back to hospital-only mode and sets `flag_missing_ww = TRUE`.

### Date Handling

- Forecast dates range from 2024-07-01 to 2025-06-30 (weekly intervals)
- Calibration period: 90 days before forecast date
- Forecast horizon: 28 days ahead
- All dates use `lubridate::ymd()` for parsing

## Code Style

- Use tidyverse style (enforced by pre-commit hooks)
- Use explicit returns in functions (`return()`)
- Keep cyclomatic complexity under 25
- Use `@autoglobal` roxygen tag for functions that use NSE (non-standard evaluation)
