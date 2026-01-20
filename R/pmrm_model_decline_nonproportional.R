#' @title Fit the non-proportional decline model.
#' @export
#' @family models
#' @description Fit the non-proportional decline model
#'   to a clinical dataset on a progressive disease.
#' @details See `vignette("models", package = "pmrm")` for details.
#' @inheritSection pmrm_model pmrm fit objects
#' @return A `pmrm` fit object of class `c("pmrm_fit_decline", "pmrm_fit")`.
#'   For details, see the "pmrm fit objects" section of this help file.
#' @inheritParams pmrm_model
#' @param initial If `initial` is a named list,
#'   then `pmrm` uses this list as the initial parameter
#'   values for the optimization.
#'   Otherwise, `pmrm` automatically computes the starting values
#'   using the method given in the `initial_method` argument (see below).
#'
#'   If `initial` is a list, then it must have the following
#'   named finite numeric elements conforming
#'   to all the true parameters defined in
#'   `vignette("models", package = "pmrm")`:
#'
#'   * `alpha`: a vector with the same length as `spline_knots`.
#'   * `theta`: a matrix with `K - 1` rows and `J - 1` columns,
#'     where `K` is the number of study arms and
#'     `J` is the number of study visits.
#'   * `gamma`: a vector with `V` elements, where `V` is the
#'     number of columns in the covariate adjustment model matrix `W`.
#'     If you are unsure of `V`, simply fit a test model
#'     (e.g. `fit <- pmrm_model_decline_nonproportional(...)`)
#'     and then check `ncol(fit$constants$W)`.
#'   * `phi`: a vector with the same length as `visit_times`
#'     (which may be different from the length of `spline_knots`).
#'   * `rho`: a vector with `J * (J - 1) / 2` elements,
#'     where `J` is the length of `visit_times`.
#'
#'   You can generate an example of the format of this list
#'   by fitting a test model
#'   (e.g. `fit <- pmrm_model_decline_nonproportional(...)`)
#'   and then extracting `fit$initial` or `fit$final`.
#' @examples
#'   set.seed(0L)
#'   simulation <- pmrm_simulate_decline_nonproportional(
#'     visit_times = seq_len(5L) - 1,
#'     gamma = c(1, 2)
#'   )
#'   fit <- pmrm_model_decline_nonproportional(
#'     data = simulation,
#'     outcome = "y",
#'     time = "t",
#'     patient = "patient",
#'     visit = "visit",
#'     arm = "arm",
#'     covariates = ~ w_1 + w_2
#'   )
#'   str(fit$estimates)
#'   names(fit)
pmrm_model_decline_nonproportional <- function(
  data,
  outcome,
  time,
  patient,
  visit,
  arm,
  covariates = ~0,
  visit_times = NULL,
  spline_knots = visit_times,
  spline_method = c("natural", "fmm"),
  reml = FALSE,
  hessian = c("divergence", "never", "always"),
  saddle = FALSE,
  control = list(eval.max = 4000L, iter.max = 4000L),
  initial_method = c("regression", "regression_control", "zero"),
  initial = NULL,
  silent = TRUE
) {
  pmrm_model(
    data = data,
    outcome = outcome,
    time = time,
    patient = patient,
    visit = visit,
    arm = arm,
    covariates = covariates,
    visit_times = visit_times,
    spline_knots = spline_knots,
    spline_method = match.arg(spline_method),
    reml = reml,
    hessian = match.arg(hessian),
    saddle = saddle,
    control = control,
    initial_method = match.arg(initial_method),
    initial = initial,
    silent = silent,
    slowing = FALSE,
    proportional = FALSE
  )
}
