pmrm_initial <- function(constants, initial_method, proportional) {
  theta <- if_any(
    proportional,
    rep(0, constants$K - 1L),
    matrix(0, nrow = constants$K - 1L, ncol = constants$J - 1L)
  )
  initial <- list(
    alpha = rep(0, length(constants$spline_knots)),
    theta = theta,
    gamma = rep(0, ncol(constants$W)),
    phi = rep(0, constants$J),
    rho = rep(0, constants$J * (constants$J - 1L) / 2L)
  )
  if (initial_method == "zero") {
    return(initial)
  }
  t <- constants$t
  y <- constants$y
  if (initial_method == "regression_control") {
    index <- constants$k == 1L
    t <- t[index]
    y <- y[index]
  }
  spline_knots <- constants$spline_knots
  initial$alpha <- stats::predict(
    stats::lm(y ~ t),
    newdata = list(t = spline_knots)
  )
  initial
}

pmrm_initial_validate <- function(initial, constants) {
  assert(
    sort(names(initial)) == c("alpha", "gamma", "phi", "rho", "theta"),
    message = paste(
      "initial must be a list with names:",
      "alpha, gamma, phi, rho, theta."
    )
  )
  for (name in names(initial)) {
    value <- initial[[name]]
    assert(
      is.vector(value) || is.matrix(value),
      is.numeric(value),
      is.finite(value),
      message = sprintf(
        "initial$%s must have numeric mode and have finite values.",
        name
      )
    )
  }
  assert(
    length(initial$alpha) == length(constants$spline_knots),
    message = paste(
      "initial$alpha must have the same length",
      "as constants$spline_knots."
    )
  )
  assert(
    initial$alpha,
    is.atomic(.),
    is.numeric(.),
    length(.) > 0L,
    is.finite(.),
    message = "initial$alpha must be a numeric vector of finite values."
  )
  assert(
    length(initial$gamma) == ncol(constants$W),
    message = paste(
      "initial$gamma must have the same length",
      "as ncol(constants$W)."
    )
  )
  assert(
    length(initial$phi) == constants$J,
    message = "initial$phi must have the same length as constants$J."
  )
  assert(
    length(initial$rho) == constants$J * (constants$J - 1L) / 2L,
    message = paste(
      "initial$rho must have length",
      "constants$J * (constants$J - 1) / 2."
    )
  )
  if (constants$proportional) {
    assert(
      is.vector(initial$theta),
      length(initial$theta) == constants$K - 1L,
      message = paste(
        "initial$theta must be a vector of length constants$K - 1",
        "for proportional models."
      )
    )
  } else {
    assert(
      is.matrix(initial$theta),
      nrow(initial$theta) == constants$K - 1L,
      ncol(initial$theta) == constants$J - 1L,
      message = paste(
        "initial$theta must be a matrix with constants$K - 1 rows",
        "and constants$J - 1 columns for non-proportional models."
      )
    )
  }
}
