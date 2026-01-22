#' @title Fitted values
#' @export
#' @family predictions
#' @description Compute the fitted values of a fitted
#'   progression model for repeated measures.
#' @details For `pmrm`, `fitted()` is much faster than `predict()`
#'   for large datasets, but the output only includes the estimates
#'   (no measures of uncertainty).
#' @return A numeric vector of fitted values corresponding to the
#'   rows of the data supplied in the `data` argument.
#' @inheritParams predict.pmrm_fit
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
#'   str(fitted(fit))
fitted.pmrm_fit <- function(
  object,
  data = object$data,
  adjust = TRUE,
  ...
) {
  f <- object$spline
  t <- object$data[[pmrm_data_labels(object$data)$time]]
  slowing <- object$constants$slowing
  W <- object$constants$W
  W_column_means <- object$constants$W_column_means
  beta_fitted <- object$estimates$beta[object$constants$index_beta_fitted]
  gamma <- object$estimates$gamma
  mu <- pmrm_mu_unadjusted(beta_fitted, f, t, slowing)
  if (adjust) {
    mu <- mu + as.numeric(W %*% gamma) - sum(W_column_means * gamma)
  }
  mu
}

#' @export
stats::fitted
