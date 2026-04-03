pmrm_objective <- function(constants, parameters) {
  RTMB::getAll(constants, parameters)
  # Define the components of the model.
  y <- RTMB::OBS(y)
  f <- pmrm_spline(x = spline_knots, y = alpha, method = spline_method)
  beta <- pmrm_beta_advector(theta, proportional)
  beta_fitted <- beta[index_beta_fitted]
  mu_unadjusted <- pmrm_mu_unadjusted(beta_fitted, f, t, slowing)
  # Reporting predictions is expensive during optimization.
  if (predict && !adjust) {
    RTMB::ADREPORT(mu_unadjusted)
  }
  mu <- mu_unadjusted + (W %*% gamma) - sum(W_column_means * gamma)
  if (predict && adjust) {
    RTMB::ADREPORT(mu)
  }
  sigma <- exp(phi)
  RTMB::ADREPORT(sigma)
  D <- RTMB::diag(sigma)
  Lambda <- RTMB::unstructured(J)$corr(rho)
  RTMB::ADREPORT(Lambda)
  Sigma <- D %*% Lambda %*% D
  RTMB::ADREPORT(Sigma)
  # Calculating the likelihood is faster if we loop through patients
  # instead of relying on sparse matrix computations.
  nll <- 0
  start <- 1L
  for (n in n_visits) {
    end <- start + n - 1L
    patient <- seq(from = start, to = end, by = 1L)
    patient_j <- j[patient]
    new <- RTMB::dmvnorm(
      x = y[patient],
      mu = mu[patient],
      Sigma = Sigma[patient_j, patient_j],
      log = TRUE
    )
    if (!is.na(new)) {
      nll <- nll - new
    }
    start <- end + 1L
  }
  # Report the marginal mean of each study arm at all the user-defined
  # time points in the `marginal_t` vector.
  marginal_beta_fitted <- beta[marginal_index_beta_fitted]
  marginal_outcome <- pmrm_mu_unadjusted(
    marginal_beta_fitted,
    f,
    marginal_t,
    slowing
  )
  RTMB::ADREPORT(marginal_outcome)
  # Report marginal change from baseline/randomization.
  # Baseline is taken to be the predicted outcome at time 0,
  # which is the same for all study arms.
  marginal_change <- marginal_outcome - f(0)
  RTMB::ADREPORT(marginal_change)
  # Report marginal treatment differences.
  marginal_control <- rep(marginal_change[marginal_k == 1L], times = K)
  marginal_effect <- marginal_change - marginal_control
  RTMB::ADREPORT(marginal_effect)
  # The objective function needs to return the negative log likelihood.
  nll
}

pmrm_spline <- function(x, y, method) {
  RTMB::splinefun(x = x, y = y, method = method)
}

pmrm_beta <- function(theta, proportional) {
  if (proportional) {
    c(0, theta)
  } else {
    rbind(0, cbind(0, theta))
  }
}

pmrm_beta_advector <- function(theta, proportional) {
  if (proportional) {
    RTMB::c.advector(0, theta)
  } else {
    RTMB::rbind.advector(0, RTMB::cbind.advector(0, theta))
  }
}

pmrm_mu_unadjusted <- function(beta_fitted, f, t, slowing) {
  if (slowing) {
    f((1 - beta_fitted) * t)
  } else {
    m1 <- f(0)
    (1 - beta_fitted) * (f(t) - m1) + m1
  }
}
