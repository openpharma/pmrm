test_that("vcov.pmrm_fit_decline()", {
  out <- vcov(fit_decline())
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(2L, 2L))
  arms <- levels(fit_decline()$data$arm)[-1L]
  expect_equal(rownames(out), arms)
  expect_equal(colnames(out), arms)
  expect_equal(
    unname(sqrt(diag(out))),
    pmrm_estimates(fit_decline(), "theta")$standard_error
  )
})

test_that("vcov.pmrm_fit_slowing()", {
  out <- vcov(fit_slowing())
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(8L, 8L))
  arms <- rep(levels(fit_slowing()$data$arm)[-1L], each = 4L)
  visits <- rep(levels(fit_slowing()$data$visit)[-1L], times = 2L)
  names <- paste(arms, visits, sep = ":")
  expect_equal(rownames(out), names)
  expect_equal(colnames(out), names)
  expect_equal(
    unname(sqrt(diag(out))),
    pmrm_estimates(fit_slowing(), "theta")$standard_error
  )
})
