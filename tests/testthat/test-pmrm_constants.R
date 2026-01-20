test_that("pmrm_compute_W() empty case", {
  set.seed(0L)
  simulation <- pmrm_simulate_decline()
  data <- pmrm_data(
    data = simulation,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm"
  )
  W <- pmrm_compute_W(data = data)
  expect_s4_class(W, "dgCMatrix")
  expect_equal(nrow(W), nrow(data))
  expect_equal(ncol(W), 0L)
})

test_that("pmrm_compute_W() nonempty case", {
  simulation <- pmrm_simulate_decline(gamma = c(-1, 1))
  data <- pmrm_data(
    data = simulation,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2
  )
  W <- pmrm_compute_W(data = data)
  expect_s4_class(W, "dgCMatrix")
  expect_equal(nrow(W), nrow(data))
  expect_equal(ncol(W), 2L)
  expect_equal(colnames(W), c("w_1", "w_2"))
  for (column in c("w_1", "w_2")) {
    expect_equal(as.numeric(W[, column]), as.numeric(data[[column]]))
  }
})

test_that("pmrm_columns_independent() vs base R equivalent", {
  set.seed(0L)
  pmrm_columns_independent_base <- function(W) {
    decomposition <- base::qr(W)
    colnames(W)[decomposition$pivot[seq_len(decomposition$rank)]]
  }
  n <- 50L
  p <- 30L
  replicate(50L, {
    W <- Matrix::rsparsematrix(n, p, density = 0.3)
    W[, sample(p, 5L)] <- W[, seq_len(5L)]
    colnames(W) <- paste0("V", seq_len(p))
    expect_equal(pmrm_columns_independent(W), pmrm_columns_independent_base(W))
  })
})
