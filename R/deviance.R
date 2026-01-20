#' @title Deviance
#' @export
#' @family model comparison
#' @description Extract the deviance
#'   (defined here as `-2 * log_likelihood`)
#'   of a fitted progression model for repeated measures.
#' @return Numeric scalar, the deviance.
#' @inheritParams predict.pmrm_fit
#' @examples
#'   set.seed(0L)
#'   simulation <- pmrm_simulate_decline(
#'     visit_times = seq_len(5L) - 1,
#'     gamma = c(1, 2)
#'   )
#'   fit <- pmrm_model_decline(
#'     data = simulation,
#'     outcome = "y",
#'     time = "t",
#'     patient = "patient",
#'     visit = "visit",
#'     arm = "arm",
#'     covariates = ~ w_1 + w_2
#'   )
#'   deviance(fit)
deviance.pmrm_fit <- function(object, ...) {
  -2 * object$metrics$log_likelihood
}

#' @export
stats::deviance
