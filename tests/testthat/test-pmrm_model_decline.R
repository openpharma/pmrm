test_that("pmrm_model_decline()", {
  set.seed(0L)
  visit_times <- seq(from = 0, to = 4, by = 1)
  for (reml in c(TRUE, FALSE)) {
    for (with_missing in c(TRUE, FALSE)) {
      if (!xor(reml, with_missing)) {
        next
      }
      simulation <- pmrm_simulate_decline(
        patients = 100,
        visit_times = visit_times,
        tau = 0,
        alpha = rep(1, length(visit_times)),
        beta = c(0, 0.1, 0.2),
        gamma = c(-1, 1),
        sigma = rep(1, length(visit_times)),
        rho = rep(0, length(visit_times) * (length(visit_times) - 1L) / 2L)
      )
      simulation$w_3 <- simulation$w_2
      for (name in c("w_2", "w_3")) {
        expect_true(is.numeric(simulation[[name]]))
        expect_equal(length(simulation[[name]]), nrow(simulation))
      }
      if (with_missing) {
        simulation$y[c(2L, 3L, 11L, 12L, 13L, 14L, 15L)] <- NA_real_
      }
      supplied_visit_times <- visit_times
      if (reml) {
        supplied_visit_times <- NULL
      }
      fit <- pmrm_model_decline(
        data = simulation,
        outcome = "y",
        time = "t",
        patient = "patient",
        visit = "visit",
        arm = "arm",
        covariates = ~ w_1 + w_2 + w_3,
        visit_times = supplied_visit_times,
        reml = reml,
        saddle = TRUE
      )
      expect_true(is.list(fit))
      expect_s3_class(fit, "pmrm_fit")
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
          covariates = ~ w_1 + w_2 + w_3
        )
      )
      expect_true(is.list(fit$constants))
      expect_equal(dim(fit$constants$W), c(nrow(fit$data), 2L))
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
      for (field in c("estimates", "standard_errors")) {
        expect_equal(sort(names(fit[[field]])), parameters)
        expect_equal(length(fit[[field]]$theta), max(fit$constants$k) - 1L)
        expect_equal(length(fit[[field]]$gamma), ncol(fit$constants$W))
        expect_equal(length(fit[[field]]$sigma), length(visit_times))
        for (parameter in c("Lambda", "Sigma")) {
          expect_true(is.numeric(fit[[field]][[parameter]]))
          expect_true(is.matrix(fit[[field]][[parameter]]))
          expect_equal(
            dim(fit[[field]][[parameter]]),
            c(length(visit_times), length(visit_times))
          )
        }
        for (parameter in c("alpha", "gamma", "theta", "sigma", "rho")) {
          expect_true(is.numeric(fit[[field]][[parameter]]))
          expect_false(anyNA(fit[[field]][[parameter]]))
          expect_true(all(is.finite(fit[[field]][[parameter]])))
        }
        for (parameter in c("Lambda", "Sigma")) {
          expect_true(all(is.finite(fit[[field]][[parameter]])))
        }
        expect_equal(fit[[field]]$beta, c(0, fit[[field]]$theta))
      }
      for (parameter in setdiff(parameters, c("beta", "Lambda"))) {
        expect_true(is.numeric(fit[[field]][[parameter]]))
        expect_true(all(is.finite(fit$standard_errors[[parameter]])))
        expect_true(all(fit$standard_errors[[parameter]] > 0))
      }
      expect_true(all(fit$estimates$sigma > sqrt(.Machine$double.eps)))
      expect_true(all(fit$standard_errors$Lambda >= 0))
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

test_that("pmrm_model_decline() divergence with hessian", {
  set.seed(0L)
  visit_times <- seq(from = 0, to = 3, by = 1)
  simulation <- pmrm_simulate_decline(
    patients = 100,
    visit_times = visit_times,
    spline_knots = visit_times,
    tau = 0,
    alpha = rep(1, length(visit_times)),
    beta = c(0, 0.1),
    gamma = c(-1, 1),
    sigma = rep(1, length(visit_times)),
    rho = rep(0, length(visit_times) * (length(visit_times) - 1L) / 2L)
  )
  simulation$y <- rep(c(-1000, 1000), length.out = nrow(simulation))
  for (hessian in c("divergence", "always")) {
    suppressWarnings(
      expect_warning(
        fit <- pmrm_model_decline(
          data = simulation,
          outcome = "y",
          time = "t",
          patient = "patient",
          visit = "visit",
          arm = "arm",
          covariates = ~ w_1 + w_2,
          visit_times = visit_times,
          reml = FALSE,
          hessian = hessian,
          control = list(eval.max = 1L, iter.max = 1L),
          saddle = TRUE
        ),
        class = "pmrm_warning"
      )
    )
  }
})

test_that("pmrm_model_decline() initial values", {
  set.seed(0L)
  visit_times <- seq_len(5L) - 1
  simulation <- pmrm_simulate_decline(
    visit_times = visit_times,
    gamma = c(1, 2)
  )
  fit <- pmrm_model_decline(
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
  fit <- pmrm_model_decline(
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
  fit <- pmrm_model_decline(
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
  fit <- pmrm_model_decline(
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
