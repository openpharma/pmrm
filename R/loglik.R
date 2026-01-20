#' @title Extract the log likelihood.
#' @export
#' @family model comparison
#' @description Extract the maximized log likelihood of a progression model
#'   for repeated measures (PMRM).
#' @return Numeric scalar, the maximized log likelihood of the fitted model.
#' @inheritParams summary.pmrm_fit
#' @examples
#'   set.seed(0L)
#'   simulation <- pmrm_simulate_decline_proportional(
#'     visit_times = seq_len(5L) - 1,
#'     gamma = c(1, 2)
#'   )
#'   fit <- pmrm_model_decline_proportional(
#'     data = simulation,
#'     outcome = "y",
#'     time = "t",
#'     patient = "patient",
#'     visit = "visit",
#'     arm = "arm",
#'     covariates = ~ w_1 + w_2
#'   )
#'   logLik(fit)
logLik.pmrm_fit <- function(object, ...) {
  object$metrics$log_likelihood
}

#' @export
stats::logLik
