test_that("pmrm_model_slowing()", {
  visit_times <- seq(from = 0, to = 4, by = 1)
  for (reml in c(TRUE, FALSE)) {
    for (with_missing in c(TRUE, FALSE)) {
      if (!xor(reml, with_missing)) {
        next
      }
      set.seed(0L)
      simulation <- pmrm_simulate_slowing(
        patients = 500,
        visit_times = visit_times,
        spline_knots = visit_times,
        tau = 0,
        gamma = c(-1, 1)
      )
      if (with_missing) {
        simulation$y[c(2L, 3L, 11L, 12L, 13L, 14L, 15L)] <- NA_real_
      }
      supplied_visit_times <- visit_times
      if (reml) {
        supplied_visit_times <- NULL
      }
      fit <- pmrm_model_slowing(
        data = simulation,
        outcome = "y",
        time = "t",
        patient = "patient",
        visit = "visit",
        arm = "arm",
        covariates = ~ w_1 + w_2,
        visit_times = supplied_visit_times,
        reml = reml,
        saddle = TRUE
      )
      expect_true(is.list(fit))
      expect_s3_class(fit, c("pmrm_fit_slowing", "pmrm_fit"))
      expect_equal(
        sort(names(fit)),
        sort(
          c(
            "data",
            "constants",
            "objective",
            "model",
            "optimization",
            "report",
            "initial",
            "final",
            "estimates",
            "standard_errors",
            "options",
            "metrics",
            "spline"
          )
        )
      )
      expect_equal(
        fit$data,
        pmrm_data(
          data = simulation,
          outcome = "y",
          time = "t",
          patient = "patient",
          visit = "visit",
          arm = "arm",
          covariates = ~ w_1 + w_2
        )
      )
      expect_true(is.list(fit$constants))
      parameters <- sort(
        c(
          "alpha",
          "beta",
          "gamma",
          "theta",
          "sigma",
          "phi",
          "rho",
          "Lambda",
          "Sigma"
        )
      )
      expect_equal(fit$final, fit$estimates[names(fit$initial)])
      # TODO: check parameters in every case when convergence improves.
      # Likely this will be when starting values improve.
      if (!reml && !with_missing) {
        for (field in c("estimates", "standard_errors")) {
          expect_equal(sort(names(fit[[field]])), parameters)
          expect_equal(
            dim(fit[[field]]$theta),
            c(fit$constants$K - 1L, fit$constants$J - 1L)
          )
          expect_equal(length(fit[[field]]$gamma), ncol(fit$constants$W))
          expect_equal(length(fit[[field]]$sigma), length(visit_times))
          for (parameter in c("alpha", "gamma", "theta", "sigma", "rho")) {
            expect_true(is.numeric(fit[[field]][[parameter]]))
          }
          for (parameter in c("Lambda", "Sigma")) {
            expect_true(is.matrix(fit[[field]][[parameter]]))
          }
          expect_equal(
            fit[[field]]$beta,
            cbind(0, rbind(0, 1 - activation(fit[[field]]$theta)))
          )
          if (fit$optimization$convergence != 0L) {
            next
          }
          for (parameter in c("alpha", "gamma", "theta", "sigma", "rho")) {
            expect_false(anyNA(fit[[field]][[parameter]]))
            expect_true(all(is.finite(fit[[field]][[parameter]])))
          }
          for (parameter in c("Lambda", "Sigma")) {
            expect_equal(
              dim(fit[[field]][[parameter]]),
              c(length(visit_times), length(visit_times))
            )
            expect_true(all(is.finite(fit[[field]][[parameter]])))
          }
        }
        if (fit$optimization$convergence == 0L) {
          for (parameter in setdiff(parameters, c("beta", "Lambda"))) {
            expect_true(is.numeric(fit[[field]][[parameter]]))
            expect_false(anyNA(fit$standard_errors[[parameter]]))
            expect_true(all(is.finite(fit$standard_errors[[parameter]])))
            expect_true(all(fit$standard_errors[[parameter]] > 0))
          }
          expect_true(all(fit$estimates$sigma > sqrt(.Machine$double.eps)))
          expect_true(all(fit$standard_errors$Lambda >= 0))
        }
      }
      expect_true(is.list(fit$metrics))
      expect_equal(
        sort(names(fit$metrics)),
        sort(
          c(
            "n_observations",
            "n_parameters",
            "log_likelihood",
            "aic",
            "bic"
          )
        )
      )
      n <- fit$metrics$n_observations
      k <- fit$metrics$n_parameters
      l <- fit$metrics$log_likelihood
      expect_equal(n, sum(!is.na(simulation$y)))
      expect_equal(k, sum(lengths(fit$initial)))
      expect_equal(l, -fit$optimization$objective)
      expect_equal(fit$metrics$aic, 2 * k - 2 * l)
      expect_equal(fit$metrics$bic, k * log(n) - 2 * l)
    }
  }
})

test_that("pmrm_model_slowing() initial values", {
  set.seed(0L)
  visit_times <- seq_len(5L) - 1
  simulation <- pmrm_simulate_slowing(
    visit_times = visit_times,
    gamma = c(1, 2)
  )
  fit <- pmrm_model_slowing(
    data = simulation,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2,
    visit_times = visit_times,
    initial_method = "zero"
  )
  expect_equal(fit$initial$alpha, rep(0, length(visit_times)))
  fit <- pmrm_model_slowing(
    data = simulation,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2,
    visit_times = visit_times,
    initial_method = "regression"
  )
  t <- fit$constants$t
  y <- fit$constants$y
  alpha <- stats::predict(stats::lm(y ~ t), newdata = list(t = visit_times))
  expect_equal(fit$initial$alpha, alpha)
  fit <- pmrm_model_slowing(
    data = simulation,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2,
    visit_times = visit_times,
    initial_method = "regression_control"
  )
  index <- fit$constants$k == 1L
  t <- t[index]
  y <- y[index]
  alpha <- stats::predict(stats::lm(y ~ t), newdata = list(t = visit_times))
  expect_equal(fit$initial$alpha, alpha)
  initial <- fit$final
  fit <- pmrm_model_slowing(
    data = simulation,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2,
    visit_times = visit_times,
    initial = initial
  )
  expect_equal(fit$initial, initial)
})
