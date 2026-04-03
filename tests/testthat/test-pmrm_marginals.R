test_that("pmrm_marginals()", {
  fits <- list(
    fit_decline_proportional(),
    fit_decline_nonproportional(),
    fit_slowing_proportional(),
    fit_slowing_nonproportional()
  )
  for (fit in fits) {
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
      for (column in c(
        "time",
        "estimate",
        "standard_error",
        "lower",
        "upper"
      )) {
        expect_true(is.numeric(out[[column]]))
      }
      for (column in c("estimate", "standard_error", "lower", "upper")) {
        if (type == "outcome") {
          expect_false(anyNA(out[[column]]))
        } else if (type == "change") {
          expect_equal(is.na(out[[column]]), out$visit == min(out$visit))
        } else {
          expect_equal(
            is.na(out[[column]]),
            out$arm == min(out$arm) | out$visit == min(out$visit)
          )
        }
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
    for (column in c("estimate", "standard_error", "lower", "upper")) {
      change[[column]][is.na(change[[column]])] <- 0
      effect[[column]][is.na(effect[[column]])] <- 0
    }
    predictions <- predict(fit, adjust = FALSE) |>
      dplyr::group_by(arm, visit) |>
      dplyr::summarize(predicted = mean(estimate), .groups = "drop")
    expect_equal(outcome$estimate, predictions$predicted)
    # Assumes the first visit is baseline/randomization:
    baseline <- outcome$estimate[outcome$visit == min(outcome$visit)]
    baseline <- rep(baseline, each = 5L)
    expect_equal(change$estimate, outcome$estimate - baseline)
    control <- change$estimate[change$arm == min(change$arm)]
    control <- rep(control, times = 3L)
    expect_equal(effect$estimate, change$estimate - control)
  }
})
