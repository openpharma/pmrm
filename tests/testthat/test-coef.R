test_that("coef.pmrm_fit_decline()()", {
  out <- coef(fit_decline())
  expect_equal(unname(out), fit_decline()$estimates$theta)
  arms <- levels(fit_decline()$data$arm)[-1L]
  expect_equal(names(out), arms)
})

test_that("coef.pmrm_fit_slowing()()", {
  out <- coef(fit_slowing())
  expect_equal(unname(out), fit_slowing()$estimates$theta)
  arms <- levels(fit_slowing()$data$arm)[-1L]
  visits <- levels(fit_slowing()$data$visit)[-1L]
  expect_equal(rownames(out), arms)
  expect_equal(colnames(out), visits)
})
