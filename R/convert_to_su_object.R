#' Convert the scores to a scoringutils object
#'
#' @param scores_raw Data.frame of scores
#' @importFrom data.table setattr as.data.table
#' @importFrom dplyr rename select
#' @returns scoringutils object
convert_to_su_object <- function(scores_raw) {
  scores <- data.table::as.data.table(scores_raw)
  class(scores) <- c("scores", class(scores))
  scores_su <- data.table::setattr(
    scores,
    "metrics",
    c(
      "wis", "overprediction", "underprediction",
      "dispersion", "bias", "interval_coverage_50",
      "interval_coverage_90", "ae_median"
    )
  )
  return(scores_su)
}

