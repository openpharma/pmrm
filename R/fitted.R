#' @title Fitted values: proportional decline model.
#' @export
#' @family estimates and predictions
#' @description Compute the fitted values of a fitted
#'   proportional decline model.
#' @details For `pmrm`, `fitted()` is much faster than `predict()`
#'   for large datasets, but the output only includes the estimates
#'   (no measures of uncertainty).
#' @return A numeric vector of fitted values corresponding to the
#'   rows of the data supplied in the `data` argument.
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
#'   str(fitted(fit))
fitted.pmrm_fit_decline <- function(
  object,
  data = object$data,
  adjust = TRUE,
  ...
) {
  t <- object$data[[pmrm_data_labels(object$data)$time]]
  W <- object$constants$W
  k <- object$constants$k
  beta <- object$estimates$beta
  gamma <- object$estimates$gamma
  m <- object$spline(t)
  m1 <- object$spline(0)
  decline <- 1 - beta[k]
  mu <- decline * (m - m1) + m1
  if (adjust) {
    mu <- mu + as.numeric(W %*% gamma)
  }
  mu
}

#' @title Fitted values: non-proportional slowing model.
#' @export
#' @family estimates and predictions
#' @description Compute the fitted values of a fitted
#'   non-proportional slowing model.
#' @details For `pmrm`, `fitted()` is much faster than `predict()`
#'   for large datasets, but the output only includes the estimates
#'   (no measures of uncertainty).
#' @return A numeric vector of fitted values corresponding to the
#'   rows of the data supplied in the `data` argument.
#' @inheritParams predict.pmrm_fit
#' @examples
#'   set.seed(0L)
#'   simulation <- pmrm_simulate_slowing(
#'     visit_times = seq_len(5L) - 1,
#'     gamma = c(1, 2)
#'   )
#'   fit <- pmrm_model_slowing(
#'     data = simulation,
#'     outcome = "y",
#'     time = "t",
#'     patient = "patient",
#'     visit = "visit",
#'     arm = "arm",
#'     covariates = ~ w_1 + w_2
#'   )
#'   str(fitted(fit))
fitted.pmrm_fit_slowing <- function(
  object,
  data = object$data,
  adjust = TRUE,
  ...
) {
  t <- object$data[[pmrm_data_labels(object$data)$time]]
  W <- object$constants$W
  W_column_means <- object$constants$W_column_means
  kj <- object$constants$kj
  beta <- object$estimates$beta
  gamma <- object$estimates$gamma
  u <- (1 - beta[kj]) * t
  mu <- object$spline(u)
  if (adjust) {
    mu <- mu + as.numeric(W %*% gamma) - sum(W_column_means * gamma)
  }
  mu
}

#' @export
stats::fitted
