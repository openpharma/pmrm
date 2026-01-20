test_that("if_any()", {
  expect_equal("first", if_any(TRUE, "first", "second"))
  expect_equal("first", if_any(c(FALSE, TRUE), "first", "second"))
  expect_equal("second", if_any(FALSE, "first", "second"))
  expect_equal("second", if_any(c(FALSE, FALSE), "first", "second"))
})
