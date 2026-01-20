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
  # time points in the `marginal` vector.
  marginal_m <- f(marginal_t)
  marginal_m1 <- rep(marginal_m[marginal_j == 1L], each = J)
  marginal_decline <- (1 - beta[marginal_k])
  marginal_change <- marginal_decline * (marginal_m - marginal_m1)
  marginal_outcome <- marginal_change + marginal_m1
  RTMB::ADREPORT(marginal_outcome)
  # Report marginal change from baseline.
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
