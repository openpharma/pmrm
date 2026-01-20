test_that("pmrm_simulate_e() uncorrelated residuals", {
  set.seed(0L)
  patients <- 1e5L
  Sigma <- diag(3L)
  e <- pmrm_simulate_e(patients, Sigma)
  expect_equal(length(e), 3e5L)
  expect_true(abs(mean(e)) < 0.01)
  expect_true(abs(sd(e) - 1) < 0.01)
  e_matrix <- t(matrix(e, ncol = patients, byrow = FALSE))
  expect_true(max(abs(cov(e_matrix) - Sigma)) < 0.01)
})

test_that("pmrm_simulate_e() correlated residuals", {
  set.seed(0L)
  patients <- 1e5L
  cholesky <- matrix(
    c(
      1,
      0,
      0,
      2.1,
      3.6,
      0,
      -2.8,
      0.7,
      0.85
    ),
    nrow = 3L,
    byrow = TRUE
  )
  Sigma <- cholesky %*% t(cholesky)
  e <- pmrm_simulate_e(patients, Sigma)
  expect_equal(length(e), 3e5L)
  expect_true(abs(mean(e)) < 0.01)
  e_matrix <- t(matrix(e, ncol = patients, byrow = FALSE))
  expect_true(max(abs(cov(e_matrix) - Sigma)) < 0.05)
})

test_that("pmrm_simulate_W()", {
  set.seed(0L)
  W <- pmrm_simulate_W(
    patients = 1e5L,
    visit_times = seq_len(3L),
    gamma = seq_len(5L)
  )
  expect_true(is.matrix(W))
  expect_equal(dim(W), c(3e5L, 5L))
  expect_equal(colnames(W), sprintf("w_%s", seq_len(5L)))
  expect_true(max(abs(mean(W))) < 0.01)
  expect_true(max(abs(cov(W) - diag(5L))) < 0.01)
})

test_that("pmrm_simulate_t()", {
  set.seed(0L)
  patients <- 1e5L
  visit_times <- c(0, 50, 190, 1006)
  tau <- 0.5
  t <- pmrm_simulate_t(
    patients = patients,
    visit_times = visit_times,
    tau = tau
  )
  expect_equal(length(t), 4e5L)
  t_matrix <- matrix(t, nrow = patients, byrow = TRUE)
  expect_true(max(abs(t_matrix[, 1L])) < sqrt(.Machine$double.eps))
  expect_true(max(abs(colMeans(t_matrix) - visit_times)) < 0.01)
  expect_true(max(abs(apply(t_matrix, 2, sd) - c(0, rep(0.5, 3L)))) < 0.01)
})

test_that("pmrm_simulate_is_baseline()", {
  patients <- 1000L
  visit_times <- seq(from = 0, to = 3, by = 1)
  baseline <- pmrm_simulate_is_baseline(patients, visit_times)
  expect_true(all(baseline[seq(
    from = 1,
    to = patients,
    by = length(visit_times)
  )]))
  for (offset in seq_len(3L)) {
    expect_false(
      all(baseline[seq(
        from = 1 + offset,
        to = patients,
        by = length(visit_times)
      )])
    )
  }
})
