test_that("fitted.pmrm_fit_decline()", {
  for (adjust in c(TRUE, FALSE)) {
    # Computed with independent R code.
    out <- fitted(fit_decline(), adjust = adjust)
    # Reported from the objective function itself.
    predictions <- predict(fit_decline(), adjust = adjust)
    expect_gt(cor(out, predictions$estimate), 0.999)
    expect_lt(max(abs(out - predictions$estimate)), 0.002)
  }
})

test_that("fitted.pmrm_fit_slowing()", {
  for (adjust in c(TRUE, FALSE)) {
    # Computed with independent R code.
    out <- fitted(fit_slowing(), adjust = adjust)
    # Reported from the objective function itself.
    predictions <- predict(fit_slowing(), adjust = adjust)
    expect_gt(cor(out, predictions$estimate), 0.999)
    expect_lt(max(abs(out - predictions$estimate)), 0.002)
  }
})
