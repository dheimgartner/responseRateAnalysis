default_data <- function() {
  keep <- c(
    "year", "authors", "survey_id", "response_rate", "response_burden_score",
    "sample_size", "flag_yes_yes", "flag_yes_no", "flag_no_no", "flag_no_yes"
  )
  rr <- response_rates[, keep]
  X <-
    rr %>%
    dplyr::mutate(y = log(response_rate / (100 - response_rate)),
                  x = response_burden_score / 1000) %>%
    dplyr::mutate(dplyr::across(tidyselect::matches("^flag"), function(x) as.numeric(x)),
                  weight = sqrt(sample_size)) %>%
    dplyr::select(y, x, weight, survey_id, tidyselect::matches("^flag"))  # keep survey_id for clustered se
  names(X) <- stringr::str_remove_all(names(X), "^flag_")
  X
}

vcov <- function(fit, cluster, type) {
  clubSandwich::vcovCR(fit, cluster = cluster, type = type)
}

# clustered_se <- function(data = NULL, fit = NULL, vcov) {
#   if (is.null(data) & !is.null(fit)) {
#     data <- model.matrix(fit)
#   }
#   sqrt(diag(data %*% vcov %*% t(data)))
# }

coef_clustered <- function(fit, vcov) {
  lmtest::coeftest(fit, vcov = vcov)
}

# critical_value <- function(fit) {
#   qt(0.975, df = fit$df.residual)
# }

add_clustered <- function(fit, cluster, type) {
  vcov_ <- vcov(fit, cluster = cluster, type = type)
  coef_clustered_ <- coef_clustered(fit = fit, vcov = vcov_)
  fit$clustered_se <- coef_clustered_[, "Std. Error"]
  fit$clustered_p <- coef_clustered_[, "Pr(>|t|)"]
  class(fit) <- c("clustered", "lm")
  fit
}

#' #' @export
#' predict.clustered <- function(object, newdata, cluster = NULL, type = NULL) {
#'   browser()
#'   tmp <- object
#'   class(tmp) <- "lm"
#'   y_hat <- as.data.frame(predict.lm(object, newdata = as.data.frame(newdata), interval = "confidence"))
#'   if (!is.null(cluster)) {
#'     vcov_ <- vcov(tmp, cluster, type)
#'     clustered_se_ <- clustered_se(newdata, object, vcov_)
#'     critical_value_ <- critical_value(object)
#'     lwr <- y_hat$fit - critical_value_ * clustered_se_
#'     upr <- y_hat$fit + critical_value_ * clustered_se_
#'     y_hat$lwr <- lwr
#'     y_hat$upr <- upr
#'   }
#'   y_hat
#' }

#' @importFrom texreg extract
extract.clustered <- function(model) {
  tmp <- model
  class(tmp) <- "lm"
  tr <- texreg:::extract(tmp)
  tr@pvalues <- unname(model$clustered_p)
  tr@se <- unname(model$clustered_se)

  ## gofs
  gof.names <- tr@gof.names
  gof <- tr@gof

  gof.names <- append(gof.names, c("LL", "AIC", "BIC"))
  gof <- append(gof, c(unclass(logLik(model))[1], AIC(model), BIC(model)))

  tr@gof.names <- gof.names
  tr@gof <- gof

  tr
}

setMethod("extract", signature = className("clustered", "responseRateAnalysis"),
          definition = extract.clustered)

#' @export
summary.clustered <- function(object, ...) {
  texreg::screenreg(object)
}
