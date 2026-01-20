test_that("AIC.pmrm_fit()", {
  expect_equal(AIC(fit_decline()), fit_decline()$metrics$aic)
  expect_equal(AIC(fit_slowing()), fit_slowing()$metrics$aic)
})
