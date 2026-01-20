test_that("assert()", {
  expect_silent(assert(TRUE))
  expect_error(assert(FALSE), class = "pmrm_error")
  expect_silent(assert(c(2, 3), . > 1, . > 0))
  expect_error(assert(2, . < 1), class = "pmrm_error")
})

test_that("assert_numeric()", {
  expect_silent(assert_numeric(123.456))
  expect_silent(assert_numeric(numeric(0L)))
  expect_silent(assert_numeric(c(1, 2)))
  expect_error(assert_nonnegative(NULL), class = "pmrm_error")
  expect_error(assert_nonnegative(NA_real_), class = "pmrm_error")
  expect_error(assert_nonnegative("123"), class = "pmrm_error")
})

test_that("assert_nonnegative()", {
  expect_silent(assert_nonnegative(123.456))
  expect_silent(assert_nonnegative(0))
  expect_error(assert_nonnegative(-123.456), class = "pmrm_error")
})

test_that("assert_positive()", {
  expect_silent(assert_positive(123.456))
  expect_error(assert_positive(0), class = "pmrm_error")
  expect_error(assert_positive(-123.456), class = "pmrm_error")
})

test_that("assert_scalar()", {
  expect_silent(assert_scalar(1))
  expect_silent(assert_scalar("a"))
  expect_error(assert_scalar(c(1, 2)), class = "pmrm_error")
  expect_error(assert_scalar(NULL), class = "pmrm_error")
  expect_error(assert_scalar(numeric(0L)), class = "pmrm_error")
})
