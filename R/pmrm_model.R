pmrm_model <- function(
  data,
  outcome,
  time,
  patient,
  visit,
  arm,
  covariates,
  visit_times,
  spline_knots,
  spline_method,
  reml,
  hessian,
  saddle,
  control,
  initial_method,
  initial,
  silent,
  slowing,
  proportional
) {
  data <- pmrm_data(
    data = data,
    outcome = outcome,
    time = time,
    patient = patient,
    visit = visit,
    arm = arm,
    covariates = covariates
  )
  if (!is.null(visit_times)) {
    assert(
      visit_times,
      is.numeric(.),
      all(is.finite(.)),
      message = "visit_times must be a numeric vector with finite elements."
    )
    assert(
      length(visit_times) ==
        length(unique(data[[pmrm_data_labels(data)$visit]])),
      message = paste(
        "The number of elements in visit_times must equal",
        "the number of visits in the data."
      )
    )
  }
  if (!is.null(spline_knots)) {
    assert(
      spline_knots,
      is.numeric(.),
      all(is.finite(.)),
      message = "spline_knots must be a numeric vector with finite elements."
    )
  }
  assert(
    reml,
    isTRUE(.) || isFALSE(.),
    message = "reml must be TRUE or FALSE."
  )
  assert(
    control,
    is.list(.),
    rlang::is_named2(.),
    message = "control argument must be a list with names for all elements."
  )
  assert(
    silent,
    isTRUE(.) || isFALSE(.),
    message = "silent must be TRUE or FALSE."
  )
  assert(
    hessian,
    is.character(.),
    length(.) == 1L,
    !anyNA(.),
    . %in% c("divergence", "never", "always"),
    message = "hessian argument must be 'divergence', 'never', or 'always'."
  )
  assert(
    saddle,
    isTRUE(.) || isFALSE(.),
    message = "saddle must be TRUE or FALSE."
  )
  assert(
    slowing,
    isTRUE(.) || isFALSE(.),
    message = "slowing must be TRUE or FALSE"
  )
  assert(
    proportional,
    isTRUE(.) || isFALSE(.),
    message = "proportional must be TRUE or FALSE"
  )
  hessian <- as.character(hessian)
  constants <- pmrm_constants(
    data = data,
    visit_times = visit_times,
    spline_knots = spline_knots,
    spline_method = spline_method,
    reml = reml,
    hessian = hessian,
    slowing = slowing,
    proportional = proportional,
    predict = FALSE,
    adjust = FALSE
  )
  if (is.null(initial)) {
    initial <- pmrm_initial(constants, initial_method, proportional)
  }
  labels <- pmrm_data_labels(data)
  options <- list(silent = silent, hessian = hessian)
  model <- RTMB::MakeADFun(
    func = function(parameters) pmrm_objective(constants, parameters),
    parameters = initial,
    random = constants$random,
    silent = silent
  )
  args <- list(
    start = model$par,
    objective = model$fn,
    gradient = model$gr,
    control = control
  )
  if (identical(hessian, "always")) {
    args$hessian <- model$he
  }
  optimization <- do.call(what = stats::nlminb, args = args)
  initially_diverged <- identical(hessian, "divergence") &&
    pmrm_model_diverged(model, optimization, reml, saddle)
  if (initially_diverged) {
    args$hessian <- model$he
    optimization <- do.call(what = stats::nlminb, args = args)
  }
  if (pmrm_model_diverged(model, optimization, reml, saddle)) {
    optimization$convergence <- 1L
  }
  if (optimization$convergence != 0L) {
    warn("{pmrm} model diverged or failed to find a true local minimum.")
  }
  options$used_hessian <- !is.null(args$hessian)
  report <- RTMB::sdreport(model, getReportCovariance = FALSE)
  final <- as.list(report, "Estimate")
  estimates <- final
  standard_errors <- as.list(report, "Std. Error")
  attr(final, "what") <- NULL
  attr(estimates, "what") <- NULL
  attr(standard_errors, "what") <- NULL
  custom <- summary(report, "report")
  estimates$beta <- pmrm_beta(estimates$theta, proportional)
  estimates$sigma <- unname(custom[rownames(custom) == "sigma", "Estimate"])
  standard_errors$beta <- pmrm_beta(standard_errors$theta, proportional)
  standard_errors$sigma <- unname(custom[
    rownames(custom) == "sigma",
    "Std. Error"
  ])
  for (name in c("Lambda", "Sigma")) {
    estimates[[name]] <- matrix(
      custom[rownames(custom) == name, "Estimate"],
      nrow = constants$J
    )
    standard_errors[[name]] <- matrix(
      custom[rownames(custom) == name, "Std. Error"],
      nrow = constants$J
    )
  }
  spline <- Vectorize(
    pmrm_spline(
      x = constants$spline_knots,
      y = estimates$alpha,
      method = constants$spline_method
    ),
    "x"
  )
  metrics <- pmrm_model_metrics(data, initial, optimization)
  fit <- list(
    data = data,
    constants = constants,
    options = options,
    objective = pmrm_objective,
    model = model,
    optimization = optimization,
    report = report,
    initial = initial,
    final = final,
    estimates = estimates,
    standard_errors = standard_errors,
    metrics = metrics,
    spline = spline
  )
  structure(fit, class = c(pmrm_class(slowing, proportional), "pmrm_fit"))
}

pmrm_model_diverged <- function(model, optimization, reml, saddle) {
  out <- optimization$convergence != 0L
  if (saddle) {
    out <- out || (!reml && any(eigen(model$he(optimization$par))$val < 0))
  }
  out
}

pmrm_model_metrics <- function(data, initial, optimization) {
  n <- nrow(data)
  k <- sum(lengths(initial))
  log_likelihood <- -optimization$objective
  aic <- 2 * k - 2 * log_likelihood
  bic <- k * log(n) - 2 * log_likelihood
  list(
    n_observations = n,
    n_parameters = k,
    log_likelihood = log_likelihood,
    aic = aic,
    bic = bic
  )
}

pmrm_class <- function(slowing, proportional) {
  paste0("pmrm_fit_", if_any(slowing, "slowing", "decline"))
}
