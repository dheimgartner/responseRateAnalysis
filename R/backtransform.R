#' Backtransforms the Response
#'
#' @param y log-transformed response
#'
#' @return numeric vector
#' @export
backtransform <- function(y) {
  y_star <- exp(y) / (1 + exp(y)) * 100
  y_star
}
