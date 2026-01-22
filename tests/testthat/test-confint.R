test_that("confint.pmrm_fit() proportional decline model", {
  fit <- fit_decline_proportional()
  out <- confint(fit, level = 0.87)
  estimates <- pmrm_estimates(fit, parameter = "theta", confidence = 0.87)
  expect_equal(rownames(out), as.character(estimates$arm))
  expect_equal(as.numeric(out[, "6.5 %"]), estimates$lower)
  expect_equal(as.numeric(out[, "93.5 %"]), estimates$upper)
})

test_that("confint.pmrm_fit() non-proportional slowing model", {
  fit <- fit_slowing_nonproportional()
  out <- confint(fit, level = 0.87)
  estimates <- pmrm_estimates(fit, parameter = "theta", confidence = 0.87)
  names <- paste(estimates$arm, estimates$visit, sep = ":")
  expect_equal(rownames(out), names)
  expect_equal(as.numeric(out[, "6.5 %"]), estimates$lower)
  expect_equal(as.numeric(out[, "93.5 %"]), estimates$upper)
})

test_that("confint.pmrm_fit() non-proportional slowing model subset", {
  fit <- fit_slowing_nonproportional()
  out <- confint(fit, parm = c("arm_3:visit_4", "arm_2:visit_3"), level = 0.87)
  estimates <- pmrm_estimates(fit, parameter = "theta", confidence = 0.87)[
    c(7L, 2L),
  ]
  names <- paste(estimates$arm, estimates$visit, sep = ":")
  expect_equal(rownames(out), names)
  expect_equal(as.numeric(out[, "6.5 %"]), estimates$lower)
  expect_equal(as.numeric(out[, "93.5 %"]), estimates$upper)
})
