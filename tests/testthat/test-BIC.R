test_that("BIC.pmrm_fit()", {
  expect_equal(BIC(fit_decline()), fit_decline()$metrics$bic)
  expect_equal(BIC(fit_slowing()), fit_slowing()$metrics$bic)
})
