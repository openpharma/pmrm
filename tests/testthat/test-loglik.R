test_that("logLik.pmrm_fit()", {
  expect_equal(logLik(fit_decline()), fit_decline()$metrics$log_likelihood)
  expect_equal(logLik(fit_slowing()), fit_slowing()$metrics$log_likelihood)
})
