#' @title Summarize a PMRM.
#' @export
#' @family model comparison
#' @description Summarize a progression model for repeated measures (PMRM).
#' @return A `tibble` with one row and columns with the following columns:
#'
#'   * `model`: name of the model: `"decline"` for proportional decline,
#'     `"slowing"` for non-proportional slowing.
#'   * `log_likelihood`: maximized log likelihood of the model fit.
#'   * `n_observations`: number of non-missing observations in the data.
#'   * `n_parameters`: number of true model parameters.
#'   * `aic`: Akaike information criterion.
#'   * `bic`: Bayesian information criterion.
#'
#'   This format is designed for easy comparison of multiple fitted models.
#' @param object A fitted model object of class `"pmrm_fit"`.
#' @param ... Not used.
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
#'   summary(fit)
summary.pmrm_fit <- function(object, ...) {
  tibble::tibble(
    model = gsub("^pmrm_fit_", "", class(object)[1L]),
    log_likelihood = object$metrics$log_likelihood,
    n_observations = object$metrics$n_observations,
    n_parameters = object$metrics$n_parameters,
    aic = object$metrics$aic,
    bic = object$metrics$bic
  )
}
