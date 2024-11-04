vcov <- function(fit, cluster, type) {
  clubSandwich::vcovCR(fit, cluster = cluster, type = type)
}

coef_clustered <- function(fit, vcov) {
  lmtest::coeftest(fit, vcov = vcov)
}


#' Add clustered standard errors and p-values
#'
#' Will result in an object of class `clustered` (and `lm`).
#'
#' @param fit linear model as returned by `lm()`
#' @param cluster vector which indicates the observations that should be clusterd.
#' @param type see `?clubSandwhich::vcovCR()`
#'
#' @return object of class `c("clustered", "lm")`
#' @export
add_clustered <- function(fit, cluster, type) {
  vcov_ <- vcov(fit, cluster = cluster, type = type)
  coef_clustered_ <- coef_clustered(fit = fit, vcov = vcov_)
  fit$clustered_se <- coef_clustered_[, "Std. Error"]
  fit$clustered_p <- coef_clustered_[, "Pr(>|t|)"]
  class(fit) <- c("clustered", "lm")
  fit
}

extract.clustered <- function(model) {
  tmp <- model
  class(tmp) <- "lm"
  tr <- texreg::extract(tmp)
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

#' Extract Method for Clustered Response Rate Analysis Fits
#'
#' This is the main method called when using functions from the `texreg-package`.
#'
#' @rdname extract.clustered
#' @aliases extract.clustered
#'
#' @param model an object of class `"clustered", "responseRateAnalysis"`.
#'
#' @return A `texreg-class` object representing the statistical model
#' @method extract clustered
#'
#' @seealso `texreg-package`, [`texreg::texreg`], [`texreg::screenreg`] and related functions.
#'
#' @details
#' The `extract` method is called internally. Higher-level functions from the
#' `texreg-package` pass arguments via `...` to `extract`.
methods::setMethod("extract", signature = className("clustered", "responseRateAnalysis"),
                   definition = extract.clustered)

#' Summarizing Clustered Response Rate Analysis Objects
#'
#' Wrapper around [`texreg::screenreg`].
#'
#' @param object an object of class `"clustered", "responseRateAnalysis"`.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Called for side-effect.
#' @export
summary.clustered <- function(object, ...) {
  texreg::screenreg(object)
}
