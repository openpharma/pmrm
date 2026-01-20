#' @title Nonlinear fixed effects: proportional decline model.
#' @export
#' @family estimates and predictions
#' @description Extract the `theta` parameter vector
#'   from the proportional decline model.
#'   `theta` measures decline in the outcome variable in each active arm
#'   relative to control.
#'   Does not include the covariate adjustment parameters `gamma`.
#' @details See `vignette("models", package = "pmrm")` for details.
#' @return A named vector of `theta` estimates with one element
#'   for each active study arm.
#'   Names indicate the study arms.
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
#'   coef(fit)
coef.pmrm_fit_decline <- function(object, ...) {
  theta <- object$estimates$theta
  names(theta) <- levels(object$data[[pmrm_data_labels(object$data)$arm]])[-1L]
  theta
}

#' @title Nonlinear fixed effects: non-proportional slowing model.
#' @export
#' @family estimates and predictions
#' @description Extract the `theta` parameter matrix
#'   from the non-proportional slowing model.
#'   `theta` measures the slowing of disease progression at each visit
#'   relative to control.
#'   Does not include the covariate adjustment parameters `gamma`.
#' @details See `vignette("models", package = "pmrm")` for details.
#' @return A named matrix of `theta` estimates
#'   with one row for each active study arm
#'   and one column for each post-baseline scheduled visit.
#'   Row and column names label the arms and visits, respectively.
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
#'   coef(fit)
coef.pmrm_fit_slowing <- function(object, ...) {
  labels <- pmrm_data_labels(object$data)
  theta <- object$estimates$theta
  rownames(theta) <- levels(object$data[[labels$arm]])[-1L]
  colnames(theta) <- levels(object$data[[labels$visit]])[-1L]
  theta
}
