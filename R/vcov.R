#' @title Nonlinear fixed effects covariance matrix: proportional
#'   decline model
#' @export
#' @family estimates and predictions
#' @description Extract the covariance matrix of the proportional
#'   decline model.
#' @return A matrix with covariance of each pair of `theta` parameters.
#'   Rows and columns are labeled by study arm.
#' @inheritParams summary.pmrm_fit
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
#'   vcov(fit)
vcov.pmrm_fit_decline <- function(object, ...) {
  full <- RTMB::sdreport(object$model, getReportCovariance = TRUE)$cov.fixed
  theta <- full[rownames(full) == "theta", colnames(full) == "theta"]
  labels <- pmrm_data_labels(object$data)
  arms <- levels(object$data[[labels$arm]])[-1L]
  rownames(theta) <- arms
  colnames(theta) <- arms
  theta
}

#' @title Nonlinear fixed effects covariance matrix: non-proportional
#'   slowing model
#' @export
#' @family estimates and predictions
#' @description Extract the covariance matrix of the non-proportional
#'   slowing model.
#' @return A matrix with covariance of each pair of `theta` parameters.
#'   Rows and columns are labeled by study arm and scheduled visit.
#' @inheritParams summary.pmrm_fit
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
#'   vcov(fit)
vcov.pmrm_fit_slowing <- function(object, ...) {
  full <- RTMB::sdreport(object$model, getReportCovariance = TRUE)$cov.fixed
  theta <- full[rownames(full) == "theta", colnames(full) == "theta"]
  labels <- pmrm_data_labels(object$data)
  arms <- levels(object$data[[labels$arm]])[-1L]
  visits <- levels(object$data[[labels$visit]])[-1L]
  original_arms <- rep(arms, times = object$constants$J - 1L)
  original_visits <- rep(visits, each = object$constants$K - 1L)
  original_names <- paste(original_arms, original_visits, sep = ":")
  rownames(theta) <- original_names
  colnames(theta) <- original_names
  # To be consistent with both pmrm_marginals() and intuition,
  # reorder the output by visit within arm
  # (as opposed to the default arm within visit).
  reordered_arms <- rep(arms, each = object$constants$J - 1L)
  reordered_visits <- rep(visits, times = object$constants$K - 1L)
  reordered_names <- paste(reordered_arms, reordered_visits, sep = ":")
  theta[reordered_names, reordered_names]
}
