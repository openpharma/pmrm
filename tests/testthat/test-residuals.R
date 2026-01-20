test_that("residuals.pmrm_fit() decline", {
  for (adjust in c(TRUE, FALSE)) {
    expect_equal(
      residuals(fit_decline(), adjust = adjust),
      fit_decline()$data$y - fitted(fit_decline(), adjust = adjust)
    )
  }
})

test_that("residuals.pmrm_fit() slowing", {
  for (adjust in c(TRUE, FALSE)) {
    expect_equal(
      residuals(fit_slowing(), adjust = adjust),
      fit_slowing()$data$y - fitted(fit_slowing(), adjust = adjust)
    )
  }
})
