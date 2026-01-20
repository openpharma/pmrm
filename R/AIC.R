#' @title Akaike information criterion (AIC)
#' @export
#' @family model comparison
#' @description Extract the Akaike information criterion (AIC)
#'   of a progression model for repeated measures (PMRM).
#' @return Numeric scalar, the Akaike information criterion (AIC)
#'   of the fitted model.
#' @inheritParams summary.pmrm_fit
#' @param k Not used. Must be `NULL`.
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
#'   AIC(fit)
AIC.pmrm_fit <- function(object, ..., k = NULL) {
  assert(is.null(k), message = "k in AIC.pmrm_fit() must be NULL.")
  object$metrics$aic
}

#' @export
stats::AIC
