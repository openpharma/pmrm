test_that("predict() on small data with decline", {
  set.seed(1L)
  visit_times <- seq_len(3L) - 1L
  simulation <- pmrm_simulate_decline(
    patients = 100,
    visit_times = visit_times,
    spline_knots = visit_times,
    tau = 0.25,
    alpha = rep(1, length(visit_times)),
    beta = c(0, 0.1, 0.2)
  )
  fit <- pmrm_model_decline(
    data = simulation,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    visit_times = visit_times,
    reml = TRUE
  )
  new_data <- pmrm_simulate_decline(
    patients = 1L,
    visit_times = fit$constants$visit_times,
    spline_knots = fit$constants$visit_times,
    tau = 0.25,
    alpha = rep(1, length(fit$constants$visit_times)),
    beta = c(0, 0.1, 0.2),
    gamma = c(-1, 1)
  )
  new_data$y <- NULL
  for (adjust in c(TRUE, FALSE)) {
    out <- predict(fit, data = new_data, adjust = adjust)
    expect_true(tibble::is_tibble(out))
    expect_equal(nrow(out), length(fit$constants$visit_times))
    expect_equal(
      sort(colnames(out)),
      sort(
        c(
          "estimate",
          "standard_error",
          "lower",
          "upper",
          "arm",
          "time",
          "visit"
        )
      )
    )
    for (name in colnames(out)) {
      if (is.numeric(out[[name]])) {
        expect_true(all(is.finite(out[[name]])))
      } else {
        expect_false(anyNA(out[[name]]))
      }
    }
    expect_equal(
      out$arm,
      ordered(rep("arm_1", nrow(out)), levels = sort(unique(fit$data$arm)))
    )
    expect_equal(
      out$visit,
      ordered(
        paste0("visit_", seq_len(nrow(out))),
        levels = sort(unique(fit$data$visit))
      )
    )
    expect_equal(out$time, new_data$t)
  }
})

test_that("predict() on small data with slowing", {
  set.seed(0L)
  fit <- fit_slowing()
  new_data <- pmrm_simulate_slowing(
    patients = 1L,
    visit_times = fit$constants$visit_times,
    spline_knots = fit$constants$visit_times,
    tau = 0.25,
    alpha = rep(1, length(fit$constants$visit_times)),
    gamma = c(-1, 1)
  )
  new_data$y <- NULL
  for (adjust in c(TRUE, FALSE)) {
    out <- predict(fit, data = new_data, adjust = adjust)
    expect_true(tibble::is_tibble(out))
    expect_equal(nrow(out), length(fit$constants$visit_times))
    expect_equal(
      sort(colnames(out)),
      sort(
        c(
          "estimate",
          "standard_error",
          "lower",
          "upper",
          "arm",
          "time",
          "visit"
        )
      )
    )
    for (name in colnames(out)) {
      if (is.numeric(out[[name]])) {
        expect_true(all(is.finite(out[[name]])))
      } else {
        expect_false(anyNA(out[[name]]))
      }
    }
    expect_equal(
      out$arm,
      ordered(rep("arm_1", nrow(out)), levels = sort(unique(fit$data$arm)))
    )
    expect_equal(
      out$visit,
      ordered(
        paste0("visit_", seq_len(nrow(out))),
        levels = sort(unique(fit$data$visit))
      )
    )
    expect_equal(out$time, new_data$t)
  }
})

test_that("predict() on all data (decline model)", {
  set.seed(0L)
  expect_no_error(
    out <- predict(
      fit_decline(),
      data = fit_decline()$data,
      adjust = TRUE,
      confidence = 0.95
    )
  )
  expect_gt(cor(out$estimate, fit_decline()$data$y), 0.8)
})

test_that("predict() on all data (slowing model)", {
  set.seed(0L)
  expect_no_error(
    out <- predict(
      fit_slowing(),
      data = fit_slowing()$data,
      adjust = TRUE,
      confidence = 0.95
    )
  )
  expect_gt(cor(out$estimate, fit_slowing()$data$y), 0.8)
})
