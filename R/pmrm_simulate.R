#' @title Simulate data.
#' @keywords internal
#' @description Simulate data from a progression model for repeated measures.
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
#'    * `mu`: expected clinical outcome at the given patient visit.
#'    * `w_*`: columns of the covariate adjustment model matrix `W`.
#'    * `e`: residuals.
#' @return A `tibble` with simulated clinical data
#'   (see the "Simulated data" section).
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
#' @param beta Treatment effect parameters. Input format and interpretation
#'   vary from model to model.
#' @param gamma Numeric vector of model coefficients for covariate adjustment.
#'   The simulation functions in `pmrm` simulate `length(gamma)` columns for
#'   the covariate adjustment model matrix `W`.
#'   Set to `numeric(0)` to omit covariates.
#' @param sigma A positive numeric vector of visit-level standard deviation
#'   parameters.
#' @param rho A finite numeric vector of correlation parameters.
#'   Must have length `J * (J - 1) / 2`, where `J` is `length(visit_times)`.
#'   The full covariance matrix `Sigma` is given by
#'   `diag(sigma) %*% RTMB::unstructured(length(sigma))$corr(rho) %*% diag(sigma)`.
pmrm_simulate <- function(
  patients,
  visit_times,
  spline_knots,
  spline_method,
  tau,
  alpha,
  beta,
  gamma,
  sigma,
  rho,
  slowing,
  proportional
) {
  assert_positive(patients, "patients must be a positive number.")
  assert_scalar(patients, "patients must be a scalar.")
  assert_numeric(visit_times, "visit_times must be numeric")
  assert(
    !anyDuplicated(visit_times),
    message = "visit_times must have all unique elements."
  )
  assert_numeric(spline_knots, "spline_knots must be numeric")
  assert(
    !anyDuplicated(spline_knots),
    message = "spline_knots must have all unique elements."
  )
  assert(
    length(visit_times) > 1,
    message = "length(visit_times) must be greater than 1."
  )
  assert(
    length(spline_knots) > 1,
    message = "length(spline_knots) must be greater than 1."
  )
  assert_numeric(tau, "tau must be numeric.")
  assert_scalar(tau, "tau must be a scalar.")
  assert_numeric(alpha, "alpha must be numeric.")
  assert(
    length(alpha) == length(spline_knots),
    message = "length(alpha) must equal length(spline_knots)."
  )
  assert_numeric(beta, message = "beta must be numeric.")
  assert(length(beta) > 0, message = "beta must be nonempty.")
  assert_numeric(gamma, "gamma must be numeric.")
  assert_positive(
    sigma,
    message = "sigma must be a numeric vector with finite postive elements."
  )
  assert(
    length(sigma) == length(visit_times),
    message = "length(sigma) must equal length(visit_times)"
  )
  assert_numeric(rho, "rho must be a numeric vector with finite elements.")
  assert(
    length(rho) == length(visit_times) * (length(visit_times) - 1L) / 2L,
    message = "rho must have length J * (J - 1) / 2, where J is length(visit_times)."
  )
  assert(
    slowing,
    isTRUE(.) || isFALSE(.),
    message = "slowing must be TRUE or FALSE"
  )
  assert(
    proportional,
    isTRUE(.) || isFALSE(.),
    message = "proportional must be TRUE or FALSE"
  )
  if (proportional) {
    assert(
      is.atomic(beta) && is.numeric(beta),
      message = "beta must be a numeric vector."
    )
    assert(beta[1L] == 0, message = "beta[1] must be 0.")
  } else {
    assert(is.matrix(beta), message = "beta must be a matrix.")
    assert(
      ncol(beta) == length(visit_times),
      message = "ncol(beta) must equal length(visit_times)"
    )
  }
  visit_times <- sort(unique(visit_times))
  spline_knots <- sort(unique(spline_knots))
  Sigma <- diag(sigma) %*%
    RTMB::unstructured(length(sigma))$corr(rho) %*%
    diag(sigma)
  e <- pmrm_simulate_e(patients = patients, Sigma = Sigma)
  W <- pmrm_simulate_W(
    patients = patients,
    visit_times = visit_times,
    gamma = gamma
  )
  t <- pmrm_simulate_t(
    patients = patients,
    visit_times = visit_times,
    tau = tau
  )
  f <- pmrm_spline(x = spline_knots, y = alpha, method = spline_method)
  i <- rep(seq_len(patients), each = length(visit_times))
  j <- rep(seq_along(visit_times), times = patients)
  K <- if_any(proportional, length(beta), nrow(beta))
  k <- (i - 1L) %% K + 1L
  beta_fitted <- beta[if_any(proportional, k, cbind(k, j))]
  mu_unadjusted <- pmrm_mu_unadjusted(beta_fitted, f, t, slowing)
  mu <- mu_unadjusted + (W %*% gamma) - sum(Matrix::colMeans(W) * gamma)
  y <- as.numeric(mu + e)
  patient <- paste0("patient_", i)
  visit <- ordered(
    paste0("visit_", j),
    levels = paste0("visit_", seq_along(visit_times))
  )
  arm <- ordered(
    paste0("arm_", k),
    levels = paste0("arm_", seq_len(K))
  )
  data <- vctrs::vec_cbind(
    patient = patient,
    visit = visit,
    arm = arm,
    i = i,
    j = j,
    k = k,
    y = y,
    t = t,
    beta = beta_fitted,
    mu = as.numeric(mu),
    e = e,
    W
  )
  tibble::as_tibble(data)
}

pmrm_simulate_e <- function(patients, Sigma) {
  e_elements <- stats::rnorm(patients * nrow(Sigma))
  as.numeric(t(chol(Sigma)) %*% matrix(e_elements, ncol = patients))
}

pmrm_simulate_W <- function(patients, visit_times, gamma) {
  if (length(gamma) < 1L) {
    return(matrix(nrow = patients * length(visit_times), ncol = 0L))
  }
  W_elements <- stats::rnorm(patients * length(visit_times) * length(gamma))
  W <- matrix(W_elements, ncol = length(gamma))
  colnames(W) <- paste0("w_", seq_len(ncol(W)))
  W
}

pmrm_simulate_t <- function(patients, visit_times, tau) {
  mean <- rep(visit_times, times = patients)
  t <- stats::rnorm(patients * length(visit_times), mean = mean, sd = tau)
  t <- abs(t)
  t[pmrm_simulate_is_baseline(patients, visit_times)] <- 0
  t
}

pmrm_simulate_is_baseline <- function(patients, visit_times) {
  rep(c(TRUE, rep(FALSE, length(visit_times) - 1L)), times = patients)
}
