test_that("deviance.pmrm_fit()", {
  expect_equal(
    deviance(fit_decline_proportional()),
    -2 * fit_decline_proportional()$metrics$log_likelihood
  )
  expect_equal(
    deviance(fit_slowing_nonproportional()),
    -2 * fit_slowing_nonproportional()$metrics$log_likelihood
  )
})
