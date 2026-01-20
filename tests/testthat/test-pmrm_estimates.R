test_that("pmrm_estimates() decline", {
  fit <- fit_decline()
  parameters <- c(
    "beta",
    "theta",
    "alpha",
    "gamma",
    "sigma",
    "phi",
    "rho",
    "Sigma",
    "Lambda"
  )
  confidence <- 0.87
  summaries <- lapply(parameters, function(x) {
    pmrm_estimates(fit, x, confidence = confidence)
  })
  columns <- c(
    "parameter",
    "visit",
    "visit_row",
    "visit_column",
    "arm",
    "index",
    "estimate",
    "standard_error",
    "lower",
    "upper"
  )
  for (element in summaries) {
    expect_true(tibble::is_tibble(element))
    expect_true(all(colnames(element) %in% columns))
    for (name in colnames(element)) {
      column <- element[[name]]
      if (is.numeric(column)) {
        has_na <- all(element$parameter == "beta") &&
          name %in% c("standard_error", "lower", "upper")
        if (!has_na) {
          expect_true(all(is.finite(column)))
        }
      } else {
        expect_true(is.character(column) || is.ordered(column))
      }
    }
    for (name in c("standard_error", "lower", "upper")) {
      element[[name]][is.na(element[[name]])] <- 0
    }
    parameter <- unique(element$parameter)
    expect_equal(element$estimate, as.numeric(fit$estimates[[parameter]]))
    expect_equal(
      element$standard_error,
      as.numeric(fit$standard_errors[[parameter]])
    )
    z <- stats::qnorm(p = (1 - confidence) / 2, lower.tail = FALSE)
    expect_equal(element$lower, element$estimate - z * element$standard_error)
    expect_equal(element$upper, element$estimate + z * element$standard_error)
  }
})

test_that("pmrm_estimates() slowing", {
  fit <- fit_slowing()
  parameters <- c(
    "beta",
    "theta",
    "alpha",
    "gamma",
    "sigma",
    "phi",
    "rho",
    "Sigma",
    "Lambda"
  )
  confidence = 0.87
  summaries <- lapply(parameters, function(x) {
    pmrm_estimates(fit, x, confidence = confidence)
  })
  columns <- c(
    "parameter",
    "visit",
    "visit_row",
    "visit_column",
    "arm",
    "index",
    "estimate",
    "standard_error",
    "lower",
    "upper"
  )
  for (element in summaries) {
    expect_true(tibble::is_tibble(element))
    expect_true(all(colnames(element) %in% columns))
    for (name in colnames(element)) {
      column <- element[[name]]
      if (is.numeric(column)) {
        has_na <- all(element$parameter == "beta") &&
          name %in% c("standard_error", "lower", "upper")
        if (!has_na) {
          expect_true(all(is.finite(column)))
        }
      } else {
        expect_true(is.character(column) || is.ordered(column))
      }
    }
  }
  for (name in c("standard_error", "lower", "upper")) {
    element[[name]][is.na(element[[name]])] <- 0
  }
  parameter <- unique(element$parameter)
  expect_equal(element$estimate, as.numeric(fit$estimates[[parameter]]))
  expect_equal(
    element$standard_error,
    as.numeric(fit$standard_errors[[parameter]])
  )
  z <- stats::qnorm(p = (1 - confidence) / 2, lower.tail = FALSE)
  expect_equal(element$lower, element$estimate - z * element$standard_error)
  expect_equal(element$upper, element$estimate + z * element$standard_error)
})
