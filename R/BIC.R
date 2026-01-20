#' @title Bayesian information criterion (BIC)
#' @export
#' @family model comparison
#' @description Extract the Bayesian information criterion (BIC)
#'   of a progression model for repeated measures (PMRM).
#' @return Numeric scalar, the Bayesian information criterion (BIC)
#'   of the fitted model.
#' @inheritParams predict.pmrm_fit
#' @param k Not used. Must be `NULL`
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
#'   BIC(fit)
BIC.pmrm_fit <- function(object, ..., k = NULL) {
  assert(is.null(k), message = "k in BIC.pmrm_fit() must be NULL.")
  object$metrics$bic
}

#' @export
stats::BIC
