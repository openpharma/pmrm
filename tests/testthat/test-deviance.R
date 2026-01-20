test_that("deviance.pmrm_fit()", {
  expect_equal(
    deviance(fit_decline()),
    -2 * fit_decline()$metrics$log_likelihood
  )
  expect_equal(
    deviance(fit_slowing()),
    -2 * fit_slowing()$metrics$log_likelihood
  )
})
