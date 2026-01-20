test_that("plot.pmrm_fit() decline", {
  out <- plot(fit_decline(), show_predictions = TRUE, facet = TRUE)
  expect_s3_class(out, "ggplot")
})

test_that("plot.pmrm_fit() slowing", {
  out <- plot(fit_slowing(), facet = FALSE)
  expect_s3_class(out, "ggplot")
})
