test_that("pmrm_marginals() decline", {
  fit <- fit_decline()
  for (type in c("outcome", "change", "effect")) {
    out <- pmrm_marginals(fit, type = type)
    expect_equal(
      sort(colnames(out)),
      sort(
        c(
          "arm",
          "visit",
          "time",
          "estimate",
          "standard_error",
          "lower",
          "upper"
        )
      )
    )
    for (column in c("time", "estimate", "standard_error", "lower", "upper")) {
      expect_true(is.numeric(out[[column]]))
      expect_false(anyNA(out[[column]]))
    }
    for (column in c("arm", "visit")) {
      expect_true(is.ordered(out[[column]]))
    }
    expect_equal(
      as.character(out$arm),
      rep(paste0("arm_", seq_len(3L)), each = 5L)
    )
    expect_equal(
      as.character(out$visit),
      rep(paste0("visit_", seq_len(5L)), times = 3L)
    )
  }
  outcome <- pmrm_marginals(fit, type = "outcome")
  change <- pmrm_marginals(fit, type = "change")
  effect <- pmrm_marginals(fit, type = "effect")
  baseline <- outcome$estimate[outcome$visit == min(outcome$visit)]
  baseline <- rep(baseline, each = 5L)
  expect_equal(change$estimate, outcome$estimate - baseline)
  control <- change$estimate[change$arm == min(change$arm)]
  control <- rep(control, times = 3L)
  expect_equal(effect$estimate, change$estimate - control)
})

test_that("pmrm_marginals() slowing", {
  fit <- fit_slowing()
  for (type in c("outcome", "change", "effect")) {
    out <- pmrm_marginals(fit, type = type)
    expect_equal(
      sort(colnames(out)),
      sort(
        c(
          "arm",
          "visit",
          "time",
          "estimate",
          "standard_error",
          "lower",
          "upper"
        )
      )
    )
    for (column in c("time", "estimate", "standard_error", "lower", "upper")) {
      expect_true(is.numeric(out[[column]]))
      expect_false(anyNA(out[[column]]))
    }
    for (column in c("arm", "visit")) {
      expect_true(is.ordered(out[[column]]))
    }
    expect_equal(
      as.character(out$arm),
      rep(paste0("arm_", seq_len(3L)), each = 5L)
    )
    expect_equal(
      as.character(out$visit),
      rep(paste0("visit_", seq_len(5L)), times = 3L)
    )
  }
  outcome <- pmrm_marginals(fit, type = "outcome")
  change <- pmrm_marginals(fit, type = "change")
  effect <- pmrm_marginals(fit, type = "effect")
  baseline <- outcome$estimate[outcome$visit == min(outcome$visit)]
  baseline <- rep(baseline, each = 5L)
  expect_equal(change$estimate, outcome$estimate - baseline)
  control <- change$estimate[change$arm == min(change$arm)]
  control <- rep(control, times = 3L)
  expect_equal(effect$estimate, change$estimate - control)
})
