test_that("AIC.pmrm_fit()", {
  expect_equal(
    AIC(fit_decline_proportional()),
    fit_decline_proportional()$metrics$aic
  )
  expect_equal(
    AIC(fit_slowing_nonproportional()),
    fit_slowing_nonproportional()$metrics$aic
  )
})
