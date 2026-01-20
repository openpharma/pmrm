test_that("pmrm_simulate_decline() without covariates", {
  data <- pmrm_simulate_decline(gamma = numeric(0L))
  expect_true(tibble::is_tibble(data))
  expect_false(any(grepl("^w_", colnames(data))))
})

test_that("pmrm_simulate_decline() with covariates", {
  set.seed(0L)
  I <- 300L
  J <- 5L
  visit_times <- seq(from = 0, to = J - 1L, by = 1)
  tau <- min(diff(visit_times)) / 4
  alpha <- c(0.15, 0.25, 0.35, 0.45, 0.55)
  beta <- c(0, 0.1, 0.2)
  gamma <- c(1.2, -3.7)
  data <- pmrm_simulate_decline(
    patients = I,
    visit_times = visit_times,
    spline_knots = visit_times,
    tau = tau,
    alpha = alpha,
    beta = beta,
    gamma = gamma
  )
  expect_true(tibble::is_tibble(data))
  expect_equal(nrow(data), I * J)
  expect_equal(
    sort(colnames(data)),
    sort(
      c(
        "patient",
        "visit",
        "arm",
        "i",
        "j",
        "k",
        "y",
        "t",
        "beta",
        "mu",
        "e",
        sprintf("w_%s", seq_len(2L))
      )
    )
  )
  for (field in colnames(data)) {
    expect_false(anyNA(data[[field]]))
  }
  expect_true(is.character(data$patient))
  for (field in c("visit", "arm")) {
    expect_true(is.ordered(data[[field]]))
  }
  expect_equal(as.character(data$visit), paste0("visit_", data$j))
  expect_equal(as.character(data$arm), paste0("arm_", data$k))
  expect_equal(
    levels(data$visit),
    paste0("visit_", seq_len(5L))
  )
  expect_equal(
    levels(data$arm),
    paste0("arm_", seq_len(3L))
  )
  expect_equal(data$i, rep(seq_len(I), each = J))
  expect_equal(data$j, rep(seq_len(J), times = I))
  expect_equal(data$k, rep(rep(seq_len(3L), each = J), times = I / 3L))
  expect_true(all(data$t >= -sqrt(.Machine$double.eps)))
  baseline <- seq(from = 1L, to = nrow(data), by = J)
  expect_true(all(abs(data$t[baseline]) < sqrt(.Machine$double.eps)))
  expect_true(all(abs(data$beta[data$k == 1L]) < sqrt(.Machine$double.eps)))
  for (k in c(2L, 3L)) {
    expect_true(
      all(
        abs(data$beta[data$k == k] - beta[k]) < sqrt(.Machine$double.eps)
      )
    )
  }
  w <- as.matrix(data[, c("w_1", "w_2")])
  expect_true(abs(mean(data$e)) < 0.05)
  expect_true(abs(sd(data$e) - 1) < 0.05)
  expect_equal(data$y, data$mu + data$e)
  f <- pmrm_spline(x = visit_times, y = alpha, method = "natural")
  i <- rep(seq_len(I), each = length(visit_times))
  j <- rep(seq_along(visit_times), times = I)
  K <- length(beta)
  k <- (i - 1L) %% K + 1L
  beta_fitted <- beta[k]
  mu_unadjusted <- pmrm_mu_unadjusted(beta_fitted, f, data$t, FALSE)
  W <- as.matrix(data[, c("w_1", "w_2")])
  mu <- mu_unadjusted + W %*% gamma - sum(Matrix::colMeans(W) * gamma)
  expect_equal(data$mu, as.numeric(mu))
})
