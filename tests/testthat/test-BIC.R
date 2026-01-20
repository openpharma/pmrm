test_that("BIC.pmrm_fit()", {
  expect_equal(
    BIC(fit_decline_proportional()),
    fit_decline_proportional()$metrics$bic
  )
  expect_equal(
    BIC(fit_slowing_nonproportional()),
    fit_slowing_nonproportional()$metrics$bic
  )
})
