simulate_decline_nonproportional <- function(scenario) {
  data <- pmrm_simulate_decline_nonproportional(
    patients = scenario$patients,
    visit_times = scenario$visit_times,
    spline_knots = scenario$spline_knots,
    tau = 0,
    alpha = scenario$alpha,
    beta = scenario$beta,
    gamma = scenario$gamma,
    sigma = scenario$sigma,
    rho = scenario$rho
  )
  fit <- pmrm_model_decline_nonproportional(
    data = data,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2,
    visit_times = scenario$visit_times,
    spline_knots = scenario$spline_knots,
    control = list(eval.max = 2000L, iter.max = 2000L)
  )
  summarize_simulation(scenario, fit)
}

simulate_decline_proportional <- function(scenario) {
  data <- pmrm_simulate_decline_proportional(
    patients = scenario$patients,
    visit_times = scenario$visit_times,
    spline_knots = scenario$spline_knots,
    tau = 0,
    alpha = scenario$alpha,
    beta = scenario$beta,
    gamma = scenario$gamma,
    sigma = scenario$sigma,
    rho = scenario$rho
  )
  fit <- pmrm_model_decline_proportional(
    data = data,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2,
    visit_times = scenario$visit_times,
    spline_knots = scenario$spline_knots,
    control = list(eval.max = 2000L, iter.max = 2000L)
  )
  summarize_simulation(scenario, fit)
}

simulate_slowing_nonproportional <- function(scenario) {
  data <- pmrm_simulate_slowing_nonproportional(
    patients = scenario$patients,
    visit_times = scenario$visit_times,
    spline_knots = scenario$spline_knots,
    tau = 0,
    alpha = scenario$alpha,
    beta = scenario$beta,
    gamma = scenario$gamma,
    sigma = scenario$sigma,
    rho = scenario$rho
  )
  fit <- pmrm_model_slowing_nonproportional(
    data = data,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2,
    visit_times = scenario$visit_times,
    spline_knots = scenario$spline_knots,
    control = list(eval.max = 2000L, iter.max = 2000L)
  )
  summarize_simulation(scenario, fit)
}

simulate_slowing_proportional <- function(scenario) {
  data <- pmrm_simulate_slowing_proportional(
    patients = scenario$patients,
    visit_times = scenario$visit_times,
    spline_knots = scenario$spline_knots,
    tau = 0,
    alpha = scenario$alpha,
    beta = scenario$beta,
    gamma = scenario$gamma,
    sigma = scenario$sigma,
    rho = scenario$rho
  )
  fit <- pmrm_model_slowing_proportional(
    data = data,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2,
    visit_times = scenario$visit_times,
    spline_knots = scenario$spline_knots,
    control = list(eval.max = 2000L, iter.max = 2000L)
  )
  summarize_simulation(scenario, fit)
}

summarize_simulation <- function(scenario, fit) {
  fit_truth <- fit
  fit_truth$estimates <- scenario
  summary_truth <- summarize_parameters(fit_truth)
  summary_fit <- summarize_parameters(fit)
  stopifnot(all(summary_fit$parameter == summary_truth$parameter))
  summary_fit |>
    mutate(
      truth = summary_truth$estimate,
      cover = (lower < truth) & (truth < upper),
      convergence = as.logical(1L - fit$optimization$convergence)
    ) |>
    mutate(cover = ifelse(is.na(cover), FALSE, cover))
}
