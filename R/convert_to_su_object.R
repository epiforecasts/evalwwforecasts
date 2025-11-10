#' Convert the scores to a scoringutils object
#'
#' @param scores_data Data.frame from Variant Nowcast Hub GitHub
#' @importFrom data.table setattr as.data.table
#' @importFrom rlang arg_match
#' @importFrom dplyr rename select
#' @returns scoringutils object
convert_to_su_object <- function(scores_data) {
  scores2 <- scores_data |>
    data.table::as.data.table()
  class(scores2) <- c("scores", class(scores2))
  scores_su <- data.table::setattr(
    scores2,
    "metrics",
    c(
      "wis", "underprediction", "overprediction", "dispersion",
      "bias", "interval_coverage_50", "interval_coverage_90",
      "ae_median"
    )
  )
  return(scores_su)
}
