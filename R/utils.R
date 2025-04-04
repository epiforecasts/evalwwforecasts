#' Suppress output and messages for code.
#' @param code Code to run quietly.
#' @return The result of running the code.
#' @export
#' @examples
#' library(cmdstanr)
#' compile_model("stan/model.stan")
#' quiet(fit_model("stan/model.stan", simulate_data_discrete()))
#' out
quiet <- function(code) {
  sink(nullfile())
  on.exit(sink())
  return(suppressMessages(code))
}
