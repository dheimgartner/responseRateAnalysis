#' @export
default_data <- function() {
  keep <- c(
    "year", "authors", "survey_id", "response_rate", "response_burden_score",
    "sample_size", "flag_yes_yes", "flag_yes_no", "flag_no_no", "flag_no_yes"
  )
  rr <- responseRateAnalysis::response_rates[, keep]
  X <-
    rr %>%
    dplyr::mutate(y = log(response_rate / (100 - response_rate)),
                  x = response_burden_score / 1000) %>%
    dplyr::mutate(dplyr::across(tidyselect::matches("^flag"), function(x) as.numeric(x)),
                  weight = sqrt(sample_size)) %>%
    dplyr::select(y, x, weight, all_of(keep))
  names(X) <- stringr::str_remove_all(names(X), "^flag_")
  X
}
