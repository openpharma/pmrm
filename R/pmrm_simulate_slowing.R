#' @title Simulate non-proportional slowing model.
#' @export
#' @family simulations
#' @description Simulate a dataset from the proportional slowing model.
#' @details Please see the vignettes for details.
#'   The "Models" vignette explains the underlying models and notation,
#'   and the "Simulation" vignette explains how [pmrm_simulate_slowing()]
#'   simulates datasets from the proportional slowing model.
#' @inheritSection pmrm_simulate_decline Simulated data
#' @return A `tibble` of clinical data simulated from the
#'   non-proportional slowing model.
#'   See the "Simulated data" section of this help file for details.
#' @inheritParams pmrm_simulate_decline
#' @param beta Numeric matrix with one row for each study arm
#'   (including the control arm)
#'   and one column for each study visit (including baseline).
#'   Each element is the non-proportional slowing since baseline
#'   of the given arm relative to the control arm.
#'   The first row and first column must be 0.
#' @examples
#'   pmrm_simulate_slowing()
pmrm_simulate_slowing <- function(
  patients = 300,
  visit_times = seq(from = 0, to = 4, by = 1),
  spline_knots = visit_times,
  spline_method = c("natural", "fmm"),
  tau = 0,
  alpha = log(spline_knots + 1),
  beta = cbind(
    0,
    rbind(
      0,
      rep(0.2, length(visit_times) - 1L),
      rep(0.3, length(visit_times) - 1L)
    )
  ),
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
    slowing = TRUE,
    proportional = FALSE
  )
}
