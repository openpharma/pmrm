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
