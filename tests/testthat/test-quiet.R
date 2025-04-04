test_that("quiet function suppresses output and messages", {
  # Test 1: Suppress print output
  expect_silent(quiet(print("This should not be visible"))) # nolint

  # Test 2: Suppress message output
  expect_silent(quiet(message("This message should be suppressed")))

  # Test 3: Return value is preserved
  result <- quiet(sum(1:10))
  expect_identical(result, 55L)

  # Test 4: Multiple outputs are suppressed
  expect_silent(quiet({
    print("Print 1") # nolint
    message("Message 1")
    cat("Cat output\n")
  }))

  # Test 5: Error is not suppressed
  expect_error(quiet(stop("This error should not be suppressed", .call = FALSE))) # nolint

  # Test 6: Complex expressions work
  complex_result <- quiet({
    x <- 10
    y <- 20
    message("Calculating...")
    x * y
  })
  expect_identical(complex_result, 200)
})
