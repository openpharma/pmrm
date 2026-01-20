test_that("confint.pmrm_fit()", {
  expect_equal(
    confint(fit_decline()),
    pmrm_estimates(fit_decline()) |>
      dplyr::select(-tidyselect::any_of(c("estimate", "standard_error")))
  )
  expect_equal(
    confint(fit_slowing()),
    pmrm_estimates(fit_slowing()) |>
      dplyr::select(-tidyselect::any_of(c("estimate", "standard_error")))
  )
})
