get_scenario_linear_decline_nonproportional <- function() {
  n_visits <- 7
  visit_times <- seq(from = 0, to = n_visits - 1, by = 1)
  theta <- rbind(
    seq(from = 0.25, to = 0.3, length.out = n_visits - 1L),
    seq(from = 0.5, to = 0.6, length.out = n_visits - 1L)
  )
  phi <- seq(from = 0, to = 0.1, length.out = n_visits)
  list(
    patients = 500L * length(beta),
    alpha = visit_times,
    beta = cbind(0, rbind(0, theta)),
    theta = theta,
    sigma = exp(phi),
    phi = phi,
    rho = seq(
      from = 0.9,
      to = 1.1,
      length.out = n_visits * (n_visits - 1L) / 2L
    ),
    gamma = c(-0.1, 0.1),
    spline_knots = visit_times,
    visit_times = visit_times
  )
}

get_scenario_linear_decline_proportional <- function() {
  n_visits <- 7
  visit_times <- seq(from = 0, to = n_visits - 1, by = 1)
  theta <- c(0.25, 0.5)
  phi <- seq(from = 0, to = 0.1, length.out = n_visits)
  list(
    patients = 500L * length(beta),
    alpha = visit_times,
    beta = c(0, theta),
    theta = theta,
    sigma = exp(phi),
    phi = phi,
    rho = seq(
      from = 0.9,
      to = 1.1,
      length.out = n_visits * (n_visits - 1L) / 2L
    ),
    gamma = c(-0.1, 0.1),
    spline_knots = visit_times,
    visit_times = visit_times
  )
}

get_scenario_linear_slowing_nonproportional <- function() {
  n_visits <- 7
  visit_times <- seq(from = 0, to = n_visits - 1, by = 1)
  theta <- rbind(
    seq(from = 0.25, to = 0.3, length.out = n_visits - 1L),
    seq(from = 0.5, to = 0.6, length.out = n_visits - 1L)
  )
  phi <- seq(from = 0, to = 0.1, length.out = n_visits)
  list(
    patients = 500L * length(beta),
    alpha = visit_times,
    beta = cbind(0, rbind(0, theta)),
    theta = theta,
    sigma = exp(phi),
    phi = phi,
    rho = seq(
      from = 0.9,
      to = 1.1,
      length.out = n_visits * (n_visits - 1L) / 2L
    ),
    gamma = c(-0.1, 0.1),
    spline_knots = visit_times,
    visit_times = visit_times
  )
}

get_scenario_linear_slowing_proportional <- function() {
  n_visits <- 7
  visit_times <- seq(from = 0, to = n_visits - 1, by = 1)
  theta <- c(0.25, 0.5)
  phi <- seq(from = 0, to = 0.1, length.out = n_visits)
  list(
    patients = 500L * length(beta),
    alpha = visit_times,
    beta = c(0, theta),
    theta = theta,
    sigma = exp(phi),
    phi = phi,
    rho = seq(
      from = 0.9,
      to = 1.1,
      length.out = n_visits * (n_visits - 1L) / 2L
    ),
    gamma = c(-0.1, 0.1),
    spline_knots = visit_times,
    visit_times = visit_times
  )
}

get_scenario_exponential_decline_nonproportional <- function() {
  scenario <- get_scenario_linear_decline_nonproportional()
  scenario$alpha <- exp(scenario$visit_times / 2)
  scenario
}

get_scenario_exponential_decline_proportional <- function() {
  scenario <- get_scenario_linear_decline_proportional()
  scenario$alpha <- exp(scenario$visit_times / 2)
  scenario
}

get_scenario_exponential_slowing_nonproportional <- function() {
  scenario <- get_scenario_linear_slowing_nonproportional()
  scenario$alpha <- exp(scenario$visit_times / 2)
  scenario
}

get_scenario_exponential_slowing_proportional <- function() {
  scenario <- get_scenario_linear_slowing_proportional()
  scenario$alpha <- exp(scenario$visit_times / 2)
  scenario
}
