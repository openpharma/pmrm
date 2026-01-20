test_that("coef.pmrm_fit() proportional decline", {
  out <- coef(fit_decline_proportional())
  expect_equal(unname(out), fit_decline_proportional()$estimates$theta)
  arms <- levels(fit_decline_proportional()$data$arm)[-1L]
  expect_equal(names(out), arms)
})

test_that("coef.pmrm_fit() non-proportional slowing", {
  out <- coef(fit_slowing_nonproportional())
  expect_equal(unname(out), fit_slowing_nonproportional()$estimates$theta)
  arms <- levels(fit_slowing_nonproportional()$data$arm)[-1L]
  visits <- levels(fit_slowing_nonproportional()$data$visit)[-1L]
  expect_equal(rownames(out), arms)
  expect_equal(colnames(out), visits)
})
