test_that("VarCorr.pmrm_fit() decline", {
  out <- VarCorr(fit_decline())
  labels <- pmrm_data_labels(fit_decline()$data)
  visits <- levels(fit_decline()$data[[labels$visit]])
  expected <- fit_decline()$estimates$Sigma
  rownames(expected) <- visits
  colnames(expected) <- visits
  expect_equal(out, expected)
})

test_that("VarCorr.pmrm_fit() slowing", {
  out <- VarCorr(fit_slowing())
  labels <- pmrm_data_labels(fit_slowing()$data)
  visits <- levels(fit_slowing()$data[[labels$visit]])
  expected <- fit_slowing()$estimates$Sigma
  rownames(expected) <- visits
  colnames(expected) <- visits
  expect_equal(out, expected)
})
