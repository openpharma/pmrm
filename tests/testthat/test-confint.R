test_that("confint.pmrm_fit()", {
  expect_equal(
    confint(fit_decline_proportional()),
    pmrm_estimates(fit_decline_proportional()) |>
      dplyr::select(-tidyselect::any_of(c("estimate", "standard_error")))
  )
  expect_equal(
    confint(fit_slowing_nonproportional()),
    pmrm_estimates(fit_slowing_nonproportional()) |>
      dplyr::select(-tidyselect::any_of(c("estimate", "standard_error")))
  )
})
