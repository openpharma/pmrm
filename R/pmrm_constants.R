pmrm_constants <- function(
  data,
  visit_times,
  spline_knots,
  spline_method,
  reml,
  hessian,
  slowing,
  proportional,
  predict,
  adjust
) {
  labels <- pmrm_data_labels(data)
  assert(
    predict,
    isTRUE(.) || isFALSE(.),
    message = "predict must be TRUE or FALSE"
  )
  assert(
    adjust,
    isTRUE(.) || isFALSE(.),
    message = "adjust must be TRUE or FALSE"
  )
  if (reml) {
    hessian <- "never"
  }
  t <- data[[labels$time]]
  i <- as.integer(data[[labels$patient]])
  I <- max(i)
  j <- as.integer(data[[labels$visit]])
  J <- max(j)
  k <- as.integer(data[[labels$arm]])
  kj <- cbind(k, j)
  K <- max(k)
  W <- pmrm_compute_W(data = data)
  if (is.null(visit_times)) {
    visit_times <- sort(as.numeric(tapply(t, j, median)))
  }
  if (is.null(spline_knots)) {
    spline_knots <- visit_times
  }
  marginal_t <- rep(visit_times, times = K)
  marginal_j <- rep(seq_along(visit_times), times = K)
  marginal_k <- rep(sort(unique(k)), each = length(visit_times))
  marginal_kj <- cbind(marginal_k, marginal_j)
  constants <- list(
    y = as.numeric(data[[labels$outcome]]),
    t = t,
    I = I,
    j = j,
    J = J,
    k = k,
    K = K,
    kj = kj,
    index_beta_fitted = if_any(proportional, k, kj),
    W = W,
    W_column_means = Matrix::colMeans(W),
    visit_times = visit_times,
    spline_knots = spline_knots,
    spline_method = spline_method,
    marginal_t = marginal_t,
    marginal_j = marginal_j,
    marginal_k = marginal_k,
    marginal_kj = marginal_kj,
    n_visits = as.integer(table(data[[labels$patient]])),
    reml = reml,
    random = if_any(reml, c("alpha", "theta", "gamma"), NULL),
    hessian = hessian,
    slowing = slowing,
    proportional = proportional,
    predict = predict,
    adjust = adjust
  )
  constants
}

pmrm_compute_W <- function(data) {
  pmrm_data_validate(data)
  labels <- pmrm_data_labels(data)
  for (name in c("arm", "visit")) {
    data[[labels[[name]]]] <- factor(data[[labels[[name]]]], ordered = FALSE)
  }
  adjusted <- as.formula(sprintf("~ 0 + %s + %s + .", labels$arm, labels$visit))
  formula <- update(labels$covariates, adjusted)
  W <- Matrix::sparse.model.matrix(formula, data = data)
  independent <- pmrm_columns_independent(W)
  grid <- expand.grid(levels(data[[labels$arm]]), levels(data[[labels$visit]]))
  colnames(grid) <- c(labels$arm, labels$visit)
  unadjusted <- as.formula(sprintf("~ 0 + %s + %s", labels$arm, labels$visit))
  arms_visits <- colnames(Matrix::sparse.model.matrix(unadjusted, data = grid))
  assert(all(arms_visits %in% independent), message = "Could not full-rank W.")
  W[, as.character(setdiff(independent, arms_visits)), drop = FALSE]
}

pmrm_columns_independent <- function(W) {
  decomposition <- Matrix::qr(W)
  R <- Matrix::qrR(decomposition, backPermute = FALSE)
  d <- abs(Matrix::diag(R))
  tolerance <- max(dim(W)) * max(d) * .Machine$double.eps
  colnames(W)[sort(decomposition@q[d > tolerance] + 1L)]
}
