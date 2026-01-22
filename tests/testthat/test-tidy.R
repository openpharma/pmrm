test_that("tidy.pmrm_fit() proportional decline model", {
  fit <- fit_decline_proportional()
  out <- tidy(fit)
  estimates <- pmrm_estimates(fit, parameter = "theta")
  expect_equal(out$term, as.character(estimates$arm))
  expect_equal(out$estimate, estimates$estimate)
  expect_equal(out$std.error, estimates$standard_error)
})

test_that("tidy.pmrm_fit() non-proportional decline model", {
  fit <- fit_decline_nonproportional()
  out <- tidy(fit)
  estimates <- pmrm_estimates(fit, parameter = "theta")
  expect_equal(out$term, paste(estimates$arm, estimates$visit, sep = ":"))
  expect_equal(out$estimate, estimates$estimate)
  expect_equal(out$std.error, estimates$standard_error)
})
