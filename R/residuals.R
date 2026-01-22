#' @title `pmrm` residuals.
#' @export
#' @family predictions
#' @description Compute the residuals (responses minus fitted values)
#'   of a fitted progression model for repeated measures.
#' @return A numeric vector of residuals corresponding to the
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
#'   str(residuals(fit))
residuals.pmrm_fit <- function(
  object,
  ...,
  data = object$data,
  adjust = TRUE
) {
  y <- object$data[[pmrm_data_labels(object$data)$outcome]]
  y - fitted(object = object, data = data, adjust = adjust, ...)
}

#' @export
stats::residuals
