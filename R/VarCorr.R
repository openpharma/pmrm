#' @title Estimated covariance matrix
#' @export
#' @family estimates and predictions
#' @description Extract estimated covariance matrix among visits
#'   within patients.
#' @return A matrix `J` rows and `J` columns, where `J` is the number
#'   of scheduled visits in the clinical trial.
#' @param x A fitted model object of class `"pmrm_fit"`
#'   produced by [pmrm_model_decline()] or [pmrm_model_slowing()].
#' @param sigma Not used for `pmrm`.
#' @param ... Not used.
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
#'   VarCorr(fit)
VarCorr.pmrm_fit <- function(x, sigma = NA, ...) {
  out <- x$estimates$Sigma
  labels <- pmrm_data_labels(x$data)
  visits <- levels(x$data[[labels$visit]])
  rownames(out) <- visits
  colnames(out) <- visits
  out
}

#' @export
nlme::VarCorr
