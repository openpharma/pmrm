test_that("error()", {
  expect_error(error("x"), class = "pmrm_error")
})

test_that("warn()", {
  expect_warning(warn("x"), class = "pmrm_warning")
})

test_that("deprecate()", {
  expect_warning(
    deprecate(
      name = "x",
      date = "y",
      version = "1.0.0",
      alternative = "none",
      condition = "warning",
      value = "x",
      frequency = "always"
    ),
    class = "pmrm_deprecate"
  )
  expect_silent(
    deprecate(
      name = "x",
      date = "y",
      version = "1.0.0",
      alternative = "none",
      condition = "warning",
      value = NULL,
      frequency = "always"
    )
  )
})
