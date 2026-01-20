test_that("plot.pmrm_fit() proportional decline", {
  out <- plot(fit_decline_proportional(), show_predictions = TRUE, facet = TRUE)
  expect_s3_class(out, "ggplot")
})

test_that("plot.pmrm_fit() non-proportional slowing", {
  out <- plot(fit_slowing_nonproportional(), facet = FALSE)
  expect_s3_class(out, "ggplot")
})
