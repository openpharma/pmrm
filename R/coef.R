#' @title Treatment effect parameters
#' @export
#' @family estimates and predictions
#' @description Extract the `theta` parameter
#'   from a progression model for repeated measures.
#' @details See `vignette("models", package = "pmrm")` for details.
#' @return For proportional models, a named vector of `theta` estimates
#'   with one element for each active study arm.
#'   For non-proportional models, a named matrix of `theta` with one
#'   row for each active study arm and one column for each
#'   post-baseline scheduled visit. Elements, rows, and columns are
#'   named with arm/visit names as appropriate.
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
#'   coef(fit)
coef.pmrm_fit <- function(object, ...) {
  theta <- object$estimates$theta
  labels <- pmrm_data_labels(object$data)
  arms <- levels(object$data[[labels$arm]])[-1L]
  visits <- levels(object$data[[labels$visit]])[-1L]
  if (object$constants$proportional) {
    names(theta) <- arms
  } else {
    rownames(theta) <- arms
    colnames(theta) <- visits
  }
  theta
}
