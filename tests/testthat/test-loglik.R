test_that("logLik.pmrm_fit()", {
  expect_equal(
    logLik(fit_decline_proportional()),
    fit_decline_proportional()$metrics$log_likelihood
  )
  expect_equal(
    logLik(fit_slowing_nonproportional()),
    fit_slowing_nonproportional()$metrics$log_likelihood
  )
})
