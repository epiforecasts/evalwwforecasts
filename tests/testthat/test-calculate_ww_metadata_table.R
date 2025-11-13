test_that("calculate_ww_metadata_table produces correct structure", {
  # Create sample wastewater data
  sample_ww_data <- tibble::tibble(
    forecast_date = as.Date("2024-07-01"),
    location_abbr = "BE",
    location_name = "Berlin",
    site = c(rep("Site1", 10), rep("Site2", 8)),
    lab = c(rep("Site1-1", 5), rep("Site1-2", 5), rep("Site2-1", 8)),
    date = c(
      seq(as.Date("2024-06-01"), as.Date("2024-06-10"), by = "day"),
      seq(as.Date("2024-06-03"), as.Date("2024-06-10"), by = "day")
    ),
    log_genome_copies_per_ml = rnorm(18, mean = 5, sd = 1),
    site_pop = c(rep(100000, 10), rep(50000, 8))
  )

  # Calculate metadata table
  result <- calculate_ww_metadata_table(sample_ww_data)

  # Test structure
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 1)

  # Test columns
  expected_cols <- c(
    "forecast_date", "location_abbr", "location_name",
    "n_sites", "pop_coverage", "avg_sampling_freq",
    "max_sampling_freq", "avg_latency", "min_latency",
    "avg_lab_changes", "data_variability"
  )
  expect_true(all(expected_cols %in% names(result)))

  # Test values
  expect_equal(result$n_sites, 2) # Should have 2 sites
  expect_equal(result$location_abbr, "BE")
  expect_equal(result$location_name, "Berlin")
  expect_equal(as.Date(result$forecast_date), as.Date("2024-07-01"))

  # Test that metrics are numeric and non-negative where appropriate
  expect_true(is.numeric(result$n_sites))
  expect_true(is.numeric(result$pop_coverage))
  expect_true(is.numeric(result$avg_sampling_freq))
  expect_true(result$n_sites >= 0)
  expect_true(result$pop_coverage > 0)
  expect_true(result$avg_sampling_freq > 0)

  # Test lab changes calculation (Site1 has 2 labs, Site2 has 1 lab)
  # Average lab changes should be (1 + 0) / 2 = 0.5
  expect_equal(result$avg_lab_changes, 0.5)
})

test_that("calculate_ww_metadata_table handles multiple forecast dates", {
  # Create sample data with multiple forecast dates
  sample_ww_data <- tibble::tibble(
    forecast_date = c(
      rep(as.Date("2024-07-01"), 10),
      rep(as.Date("2024-10-21"), 10)
    ),
    location_abbr = "BE",
    location_name = "Berlin",
    site = rep(c("Site1", "Site2"), 10),
    lab = rep(c("Site1-1", "Site2-1"), 10),
    date = c(
      seq(as.Date("2024-06-01"), as.Date("2024-06-10"), by = "day"),
      seq(as.Date("2024-09-21"), as.Date("2024-09-30"), by = "day")
    ),
    log_genome_copies_per_ml = rnorm(20, mean = 5, sd = 1),
    site_pop = rep(c(100000, 50000), 10)
  )

  result <- calculate_ww_metadata_table(sample_ww_data)

  # Should have 2 rows (one for each forecast date)
  expect_equal(nrow(result), 2)
  expect_equal(
    sort(as.Date(result$forecast_date)),
    sort(as.Date(c("2024-07-01", "2024-10-21")))
  )
})

test_that("calculate_ww_metadata_table handles state_pop_data correctly", {
  # Create sample wastewater data
  sample_ww_data <- tibble::tibble(
    forecast_date = as.Date("2024-07-01"),
    location_abbr = "BE",
    location_name = "Berlin",
    site = rep("Site1", 10),
    lab = rep("Site1-1", 10),
    date = seq(as.Date("2024-06-01"), as.Date("2024-06-10"), by = "day"),
    log_genome_copies_per_ml = rnorm(10, mean = 5, sd = 1),
    site_pop = rep(100000, 10)
  )

  # Create state population data
  state_pop_data <- tibble::tibble(
    location_abbr = "BE",
    state_pop = 3000000
  )

  result <- calculate_ww_metadata_table(sample_ww_data, state_pop_data)

  # Population coverage should be site_pop / state_pop = 100000 / 3000000
  expect_true(abs(result$pop_coverage - 100000 / 3000000) < 0.001)
})

test_that("calculate_ww_metadata_table handles empty data", {
  empty_data <- tibble::tibble(
    forecast_date = as.Date(character()),
    location_abbr = character(),
    location_name = character(),
    site = character(),
    lab = character(),
    date = as.Date(character()),
    log_genome_copies_per_ml = numeric(),
    site_pop = numeric()
  )

  result <- calculate_ww_metadata_table(empty_data)

  # Should return an empty data frame with correct structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})
