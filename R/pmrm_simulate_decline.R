#' @title Simulate proportional decline model.
#' @export
#' @family simulations
#' @description Simulate a dataset from the proportional decline model.
#' @details Please see the vignettes for details.
#'   The "Models" vignette explains the underlying models and notation,
#'   and the "Simulation" vignette explains how [pmrm_simulate_decline()]
#'   simulates datasets from the proportional decline model.
#' @section Simulated data:
#'   The datasets returned from the simulation functions
#'   have one row per patient visit and the following columns
#'   which conform to the notation from `vignette("models", package = "pmrm")`:
#'
#'    * `patient`: Character vector of patient ID labels.
#'    * `visit`: Ordered factor of clinical visits with labels included.
#'      `min(visit)` indicates the baseline visit.
#'    * `arm`: Ordered factor of study arms with visits included.
#'      `min(arm)` indicates the control arm.
#'    * `i`: integer ID of each patient.
#'    * `j`: integer ID of each clinical visit.
#'      `j == 1` at baseline.
#'    * `k`: integer ID of the study arm of patient `i`.
#'      `k == 1` for the control arm.
#'    * `y`: clinical outcomes.
#'    * `t`: observed continuous time since baseline.
#'    * `beta`: the scalar component of the treatment effect parameter
#'      `beta` defined for patient `i`.
#'    * `e`: residuals.
#'    * `w_*`: columns of the covariate adjustment model matrix `W`.
#' @return A `tibble` of clinical data simulated from the
#'   proportional decline model.
#'   See the "Simulated data" section of this help file for details.
#' @param patients Positive integer scalar,
#'   total number of patients in the output dataset.
#'   Patients are allocated (roughly) uniformly across the study arms.
#' @param visit_times Numeric vector, the continuous scheduled time
#'   after randomization of each study visit.
#' @param spline_knots Numeric vector of spline knots on the continuous scale,
#'   including boundary knots.
#' @param spline_method Character string, spline method to use for the base model.
#'   Must be `"natural"` or `"fmm"`.
#'   See [stats::splinefun()] for details.
#' @param tau Positive numeric scalar, standard deviation for jittering
#'   the simulated time points.
#'   Defaults to 0 so that the observed continuous times are just the
#'   scheduled visit times.
#' @param alpha Numeric vector of spline coefficients for simulating
#'   the mean function `f(t_{ij} | spline_knots, alpha)`.
#'   Must have `length(spline_knots)` elements.
#' @param beta Numeric vector with one element per study arm
#'   (including the control arm).
#'   Each element is the reduction in proportional decline of the
#'   corresponding arm relative to the control arm.
#'   The first element corresponds to the control arm and therefore must be 0.
#' @param gamma Numeric vector of model coefficients for covariate adjustment.
#'   [pmrm_simulate_decline()] simulates `length(gamma)` columns for
#'   the covariate adjustment model matrix `W`.
#'   Set to `numeric(0)` to omit covariates.
#' @param sigma A positive numeric vector of visit-level standard deviation
#'   parameters.
#' @param rho A finite numeric vector of correlation parameters.
#'   Must have length `J * (J - 1) / 2`, where `J` is `length(visit_times)`.
#'   The full covariance matrix `Sigma` is given by
#'   `diag(sigma) %*% RTMB::unstructured(length(sigma))$corr(rho) %*% diag(sigma)`.
#' @examples
#'   pmrm_simulate_decline()
pmrm_simulate_decline <- function(
  patients = 300,
  visit_times = seq(from = 0, to = 4, by = 1),
  spline_knots = visit_times,
  spline_method = c("natural", "fmm"),
  tau = 0,
  alpha = log(spline_knots + 1),
  beta = c(0, 0.1, 0.2),
  gamma = numeric(0L),
  sigma = rep(1, length(visit_times)),
  rho = rep(0, length(visit_times) * (length(visit_times) - 1L) / 2L)
) {
  pmrm_simulate(
    patients = patients,
    visit_times = visit_times,
    spline_knots = spline_knots,
    spline_method = match.arg(spline_method),
    tau = tau,
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    sigma = sigma,
    rho = rho,
    slowing = FALSE,
    proportional = TRUE
  )
}
