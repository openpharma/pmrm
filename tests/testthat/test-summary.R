test_that("summary.pmrm_fit() decline", {
  out <- summary(fit_decline())
  expect_true(tibble::is_tibble(out))
  expect_equal(dim(out), c(1L, 6L))
  expect_equal(out$model, "decline")
  for (field in names(fit_decline()$metrics)) {
    expect_equal(out[[field]], fit_decline()$metrics[[field]])
  }
})

test_that("summary.pmrm_fit() slowing", {
  out <- summary(fit_slowing())
  expect_true(tibble::is_tibble(out))
  expect_equal(dim(out), c(1L, 6L))
  expect_equal(out$model, "slowing")
  for (field in names(fit_slowing()$metrics)) {
    expect_equal(out[[field]], fit_slowing()$metrics[[field]])
  }
})
