#' default_data <- function() {
#'   keep <- c(
#'     "year", "authors", "survey_id", "response_rate", "response_burden_score",
#'     "sample_size", "flag_yes_yes", "flag_yes_no", "flag_no_no", "flag_no_yes"
#'   )
#'   rr <- response_rates[, keep]
#'   X <-
#'     rr %>%
#'     dplyr::mutate(y = log(response_rate / (100 - response_rate)),
#'                   x = response_burden_score / 1000) %>%
#'     dplyr::mutate(dplyr::across(tidyselect::matches("^flag"), function(x) as.numeric(x)),
#'                   weight = sqrt(sample_size)) %>%
#'     dplyr::select(y, x, weight, survey_id, tidyselect::matches("^flag"))  # keep survey_id for clustered se
#'   names(X) <- stringr::str_remove_all(names(X), "^flag_")
#'   X
#' }
#'
#' vcov <- function(fit, cluster, type = "CR2") {
#'   # sandwich::vcovCL(fit, cluster = ~survey_id)  # no small sample correction
#'   clubSandwich::vcovCR(fit, cluster = cluster, type = type)
#' }
#'
#' clustered_standard_errors <- function(fit, cluster, type = "CR2") {
#'   return(lmtest::coeftest(fit, vcov = vcov(fit, cluster, type)))
#' }
#'
#' estimate_pooled <- function(data = NULL, weights = NULL) {
#'   if (is.null(data)) {
#'     data <- default_data()
#'   }
#'   if (is.null(weights)) {
#'     weights <- data$weight
#'   }
#'   fit <- lm(y ~ 0 + x + yes_yes + yes_no + no_no + no_yes,
#'             data = data,
#'             weights = weights)
#'   fit
#' }
#'
#' estimate_flag <- function(data = NULL, weights = NULL,
#'                           flag = c("yes_yes", "yes_no", "no_no", "no_yes")) {
#'   flag <- match.arg(flag)
#'   if (is.null(data)) {
#'     data <- default_data()
#'   }
#'
#'   data <- data[data[[flag]] == 1, ]
#'   data <- data[, c("y", "x", "weight", "survey_id")]  # keep survey_id for clustered se
#'
#'   if (is.null(weights)) {
#'     weights <- data$weight
#'   }
#'   fit <- lm(y ~ x,
#'             data = data,
#'             weights = weights)
#'   fit
#' }
#'
#' estimate <- function(data = NULL, weights = NULL, flag = NULL, cluster, type = "CR2") {
#'   if (is.null(flag)) {
#'     fit <- estimate_pooled(data, weights)
#'   } else {
#'     fit <- estimate_flag(data, weights, flag)
#'   }
#'
#'   ## clustered se
#'   fit$clustered <- clustered_standard_errors(fit, cluster, type)
#'
#'   cls <- class(fit)
#'   class(fit) <- c("responseRateModel", cls)
#'   return(fit)
#' }
#'
#' #' @importFrom texreg extract
#' #' @export
#' extract.responseRateModel <- function(model) {
#'   class(model) <- "lm"  # hacky
#'   tr <- texreg::extract(model)
#'   tr@pvalues <- unname(model$clustered[, "Pr(>|t|)"])
#'   tr@se <- unname(model$clustered[, "Std. Error"])
#'   return(tr)
#' }
#'
#' setMethod("extract", signature = className("responseRateModel", "responseRateAnalysis"),
#'           definition = extract.responseRateModel)
#'
#' generate_table <- function(data = NULL, cluster, type = "CR2", tr = texreg::screenreg, ...) {
#'   m0 <- estimate(data = data, cluster = cluster, type = type)
#'   m1 <- estimate(data = data, flag = "yes_yes", cluster = cluster, type = type)
#'   m2 <- estimate(data = data, flag = "yes_no", cluster = cluster, type = type)
#'   m3 <- estimate(data = data, flag = "no_no", cluster = cluster, type = type)
#'   m4 <- estimate(data = data, flag = "no_yes", cluster = cluster, type = type)
#'   models <- list(
#'     Pooled = m0,
#'     `Yes, yes` = m1,
#'     `Yes, no` = m2,
#'     `No, no` = m3,
#'     `No, yes` = m4
#'   )
#'   tr(models, ...)
#' }
#'
#' transform <- function(y) {
#'   log(y / (100 - y))
#' }
#'
#' backtransform <- function(y) {
#'   y_star <- exp(y) / (1 + exp(y)) * 100
#'   y_star
#' }
#'
#' pred_cluster_se <- function(fit, vcov) {
#'   X <- model.matrix(fit)
#'   se <- sqrt(rowSums((X %*% vcov) * X))
#'   se
#' }
#'
#' pred <- function(fit) {
#'   mf <- model.frame(fit)
#'   x <- mf[, "x"]
#'   y <- backtransform(mf[, "y"])
#'   cl_se <- pred_cluster_se(fit, vcov(fit))
#'   cl_pred <-
#'     data.frame(fit = predict(fit)) %>%
#'     dplyr::mutate(cl_se = cl_se,
#'                   ci_lower = fit - 1.96 * cl_se,
#'                   ci_upper = fit + 1.96 * cl_se)
#'
#'   cl_pred_backtrans <-
#'     cl_pred %>%
#'     dplyr::mutate(pred_rr = backtransform(fit),
#'                   ci_lower = backtransform(ci_lower),
#'                   ci_upper = backtransform(ci_upper)) %>%
#'     dplyr::select(pred_rr, ci_lower, ci_upper)
#'
#'   cl_pred_backtrans$x <- x
#'   cl_pred_backtrans$y <- y
#'
#'   cls <- class(cl_pred_backtrans)
#'   class(cl_pred_backtrans) <- c("responseRatePrediction", cls)
#'   cl_pred_backtrans
#' }
#'
#' #' @importFrom ggplot2 ggplot aes geom_point geom_line geom_ribbon
#' #' @importFrom scales label_percent
#' plot_default <- function(data = NULL) {
#'   tmp <- c("yes_yes", "yes_no", "no_no", "no_yes")
#'   ests <- lapply(tmp, function(x) {
#'     p <- pred(estimate(data = data, flag = x))
#'     p$flag <- x
#'     p
#'   })
#'
#'   dat <- reduce(ests, rbind)
#'
#'   dat %>%
#'     ggplot(aes(x = x, group = flag, shape = flag, col = flag)) +
#'     geom_point(aes(y = y)) +
#'     geom_line(aes(y = pred_rr), show.legend = FALSE) +
#'     geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.3, show.legend = FALSE) +
#'     scale_y_continuous(labels = scales::label_percent(scale = 1)) +
#'     labs(x = "Response burden score", y = "Response rate",
#'          col = "Recruitment, incentive", shape = "Recruitment, incentive")
#' }
