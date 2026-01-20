#' @title Confidence intervals of parameters
#' @export
#' @family model comparison
#' @description Compute confidence intervals of the family of model
#'   parameters specified in `parm`.
#' @details See `vignette("models", package = "pmrm")` for details.
#' @return The `tibble` returned by [pmrm_estimates()], except
#'   without the columns with point estimates or standard errors.
#' @inheritParams predict.pmrm_fit
#' @param parm Character string, name of a family of parameters
#'   to compute confidence intervals.
#' @param level Numeric scalar between 0 and 1, confidence level.
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
#'   confint(fit)
confint.pmrm_fit <- function(
  object,
  parm = c(
    "theta",
    "beta",
    "alpha",
    "gamma",
    "sigma",
    "phi",
    "rho",
    "Sigma",
    "Lambda"
  ),
  level = 0.95,
  ...
) {
  parm <- match.arg(parm)
  pmrm_estimates(fit = object, parameter = parm, confidence = level) |>
    dplyr::select(-tidyselect::any_of(c("estimate", "standard_error")))
}

#' @export
stats::confint
