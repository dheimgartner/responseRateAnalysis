#' Loads and Prepares the Data for Estimation
#'
#' Loads and prepares the [`response_rates`] data for estimation. It selects the
#' input variables, computes the weights, rescales the responsee burden score and
#' log transforms the response rate.
#'
#' @return Data frame
#' @export
default_data <- function() {
  keep <- c(
    "year", "authors", "survey_id", "response_rate", "response_burden_score",
    "sample_size", "RxI", "RxNI", "NRxNI", "NRxI"
  )
  rr <- responseRateAnalysis::response_rates[, keep]
  X <- rr
  X$y <- with(rr, log(response_rate / (100 - response_rate)))
  X$x <- with(rr, response_burden_score / 1000)
  X$weight <- sqrt(rr$sample_size)
  X$RxI <- as.numeric(rr$RxI)
  X$RxNI <- as.numeric(rr$RxNI)
  X$NRxNI <- as.numeric(rr$NRxNI)
  X$NRxI <- as.numeric(rr$NRxI)

  X[, c("y", "x", "weight", keep)]
}
