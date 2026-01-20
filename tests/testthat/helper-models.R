pmrm_test <- new.env(parent = emptyenv())

fit_decline_nonproportional <- function() {
  if (!is.null(pmrm_test$fit_decline)) {
    return(pmrm_test$fit_decline)
  }
  set.seed(0L)
  visit_times <- seq_len(5L) - 1
  simulation <- pmrm_simulate_decline_nonproportional(
    visit_times = visit_times,
    gamma = c(1, 2)
  )
  simulation$w_1 <- simulation$w_1 + 5
  fit <- pmrm_model_decline_nonproportional(
    data = simulation,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2,
    visit_times = visit_times
  )
  pmrm_test$fit_decline <- fit
  fit
}

fit_decline_proportional <- function() {
  if (!is.null(pmrm_test$fit_decline)) {
    return(pmrm_test$fit_decline)
  }
  set.seed(0L)
  visit_times <- seq_len(5L) - 1
  simulation <- pmrm_simulate_decline_proportional(
    visit_times = visit_times,
    gamma = c(1, 2)
  )
  simulation$w_1 <- simulation$w_1 + 5
  fit <- pmrm_model_decline_proportional(
    data = simulation,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2,
    visit_times = visit_times
  )
  pmrm_test$fit_decline <- fit
  fit
}

fit_slowing_nonproportional <- function() {
  if (!is.null(pmrm_test$fit_slowing)) {
    return(pmrm_test$fit_slowing)
  }
  set.seed(0L)
  visit_times <- seq_len(5L) - 1
  simulation <- pmrm_simulate_slowing_nonproportional(
    visit_times = visit_times,
    gamma = c(1, 2)
  )
  simulation$w_2 <- simulation$w_2 + 5
  fit <- pmrm_model_slowing_nonproportional(
    data = simulation,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2,
    visit_times = visit_times
  )
  pmrm_test$fit_slowing <- fit
  fit
}

fit_slowing_proportional <- function() {
  if (!is.null(pmrm_test$fit_slowing)) {
    return(pmrm_test$fit_slowing)
  }
  set.seed(0L)
  visit_times <- seq_len(5L) - 1
  simulation <- pmrm_simulate_slowing_proportional(
    visit_times = visit_times,
    gamma = c(1, 2)
  )
  simulation$w_2 <- simulation$w_2 + 5
  fit <- pmrm_model_slowing_proportional(
    data = simulation,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2,
    visit_times = visit_times
  )
  pmrm_test$fit_slowing <- fit
  fit
}
