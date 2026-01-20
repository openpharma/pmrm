#' @title Simulate proportional slowing model.
#' @export
#' @family simulations
#' @description Simulate a dataset from the proportional slowing model.
#' @details See `vignette("models", package = "pmrm")` for details.
#' @inheritSection pmrm_simulate Simulated data
#' @return A `tibble` of clinical data simulated from the slowing model.
#'   See the "Simulated data" section of this help file for details.
#' @inheritParams pmrm_simulate
#' @param beta Numeric vector with one element per study arm
#'   (including the control arm).
#'   See `vignette("models", package = "pmrm")` for details on this parameter.
#' @examples
#'   pmrm_simulate_slowing_proportional()
pmrm_simulate_slowing_proportional <- function(
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
    slowing = TRUE,
    proportional = TRUE
  )
}
