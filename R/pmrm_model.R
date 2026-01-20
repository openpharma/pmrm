#' @title Fit a progression model for repeated measures.
#' @keywords internal
#' @description Fit a progression model for repeated measures.
#' @section pmrm fit objects:
#'  A `"pmrm_fit"` object is a classed list returned by modeling functions
#'  such as [pmrm_model_decline()] and [pmrm_model_slowing()].
#'  It has the following named elements:
#'
#'  * `data`: a `tibble`, the input data with
#'    the missing outcomes removed and the remaining rows
#'    sorted by patient and visit within patient.
#'    The data has a special `"pmrm_data"` class and should
#'    not be modified by the user.
#'  * `constants`: a list of fixed quantities from the data
#'    that the objective function uses in the optimization.
#'    Most of these quantities are defined in the modeling and
#'    simulation vignettes in the `pmrm` package.
#'    `n_visits` is a positive integer vector with the number
#'    of non-missing outcomes for each patient.
#'  * `options`: a list of low-level model-fitting options for `RTMB`.
#'  * `objective`: the objective function for the optimization.
#'    Returns the minus log likelihood of the model.
#'    The arguments are (1) a list of constants,
#'    and (2) a list of model parameters.
#'    Both arguments have strict formatting requirements.
#'    For (1), see the `constants` element of the fitted model object.
#'    For (2), see `initial` or `final`.
#'    `model$fn` (from the `model` element of the fitted model object)
#'    contains a copy of the objective function that only takes
#'    a parameter list.
#'    (The constants are in the closure of `model$fn`.)
#'  * `model`: model object returned by [RTMB::MakeADFun()]
#'    with the compiled objective function and gradient.
#'    The elements can be supplied to an optimization routine in R
#'    such as [stats::nlminb()].
#'  * `optimization`: the object returned by [stats::nlminb()] to perform
#'    the optimization that estimates the parameters.
#'    `optimization$convergence` equals 0
#'    if an only if the model converges.
#'  * `report`: object returned by [RTMB::sdreport()] which has information
#'    on the standard deviations of model parameters.
#'  * `initial`: a list of model parameters initial values.
#'    Includes true parameters like `theta` and `alpha` but does not
#'    include derived parameters like `beta` or `sigma`.
#'    You can supply your own list of similarly formatted initial values
#'    to the `initial` argument of the modeling function
#'    ([pmrm_model_decline()] or [pmrm_model_slowing()]).
#'  * `final`: a list of model parameter estimates after optimization,
#'    but not including derived parameters like `beta` or `sigma`.
#'    The format is exactly the same as `initial` (see above)
#'    to help deal with divergent model fits.
#'    If your model fit diverged and you want to try resume the optimization
#'    with slightly better values, you can modify values in `final`
#'    and supply the result to the `initial` argument of the modeling function
#'    ([pmrm_model_decline()] or [pmrm_model_slowing()].
#'  * `estimates`: a full list of parameter estimates, including derived
#'    parameters
#'  * `standard_errors`: a list of parameter standard errors.
#'  * `metrics`: a list of high-level model metrics, including:
#'
#'    * `n_observations`: positive integer scalar,
#'       number of non-missing observations in the data.
#'    * `n_parameters`: positive integer scalar,
#'       number of model parameters in the data.
#'       Includes true parameters like `theta`
#'       but excludes downstream functions
#'       of parameters such as `beta`.
#'    * `log_likelihood`: numeric scalar,
#'      the maximized log likelihood of the fitted model.
#'    * `aic`: numeric scalar, the Akaike information
#'      criterion of the fitted model.
#'    * `bic`: numeric scalar, the Bayesian information
#'        criterion of the fitted model.
#'
#'  * `spline`: a vectorized function that accepts continuous time `x`
#'    and returns the value of the fitted spline `f(x | spline_knots, alpha)` at time `x`
#'    given the user-specified knots `spline_knots` and the
#'    maximum likelihood estimates of `alpha`.
#'    Useful for diagnosing strange behavior in the fitted spline.
#'    If the spline behaves oddly,
#'    especially extrapolating beyond the range of the
#'    time points, please consider adjusting
#'    the knots `spline_knots` or the initial values of `alpha`
#'    when refitting the model.
#' @return A `"pmrm_fit"` object
#'   (see the "pmrm fit objects" section for details).
#' @inheritParams RTMB::MakeADFun
#' @inheritParams pmrm_data
#' @inheritParams pmrm_simulate_decline
#' @param data A data frame or `tibble` of clinical data.
#' @param visit_times Numeric vector, the continuous scheduled time
#'   of each study visit (since randomization).
#'   If `NULL`, each visit time is automatically set set to the median
#'   of the observed times at categorical visit in the data.
#' @param spline_knots Numeric vector of spline knots on the continuous scale,
#'   including boundary knots.
#' @param reml `TRUE` to fit the model with restricted maximum likelihood
#'   (REML), which involves integrating out fixed effects.
#'   `FALSE` to use unrestricted maximum likelihood.
#'   If `reml` is `TRUE`, then `hessian` is automatically set to `"never"`.
#' @param hessian Character string controlling
#'   when to supply the Hessian matrix
#'   of the objective function to the optimizer [stats::nlminb()].
#'   Supplying the Hessian usually slows down optimization but may
#'   improve convergence in some cases, particularly saddle points in the
#'   objective function.
#'
#'   The `hessian` argument is automatically set to `"never"`
#'   whenever `reml` is `TRUE`.
#'
#'   The `hessian` argument must be one of the following values:
#'
#'   * `"divergence"`: first try the model
#'     without supplying the Hessian.
#'     Then if the model does not converge,
#'     retry while supplying the Hessian.
#'   * `"never"`: fit the model only once and do not supply
#'     the Hessian to [stats::nlminb()].
#'   * `"always"`: fit the model once and supply the Hessian to
#'     [stats::nlminb()].
#' @param saddle `TRUE` to check if the optimization hit a saddle point,
#'   and if it did, treat the model fit as if it diverged.
#'   `FALSE` to skip this check for the sake of speed.
#' @param control A named list of control parameters passed directly to the
#'   `control` argument of [stats::nlminb()].
#' @param initial_method Character string, name of the method
#'   for computing initial values.
#'   Ignored unless `initial` is `NULL`.
#'   Must have one of the following values:
#'
#'  * `"regression"`: sets the spline vertical distances `alpha`
#'    to the fitted values at the knots of a simple linear regression
#'    of the responses versus continuous time.
#'    Sets all the other true model parameters to 0.
#'  * `"regression_control"`: like `"regression"` except we only
#'    use the data from the control group.
#'    Sets all the other true model parameters to 0.
#'  * `"zero"`: sets all true model parameters to 0, including `alpha`.
#' @param initial If `initial` is a named list,
#'   then `pmrm` uses this list as the initial parameter
#'   values for the optimization.
#'   Otherwise, `pmrm` automatically computes the starting values
#'   using the method given in the `initial_method` argument (see below).
#'
#'   If `initial` is a list, then it must have the following
#'   named finite numeric elements conforming
#'   to all the true parameters defined in
#'   `vignette("models", package = "pmrm")`:
#'
#'  * `alpha`: a vector with the same length as `spline_knots`.
#'  * `theta`: for proportional models,
#'    a vector with `K - 1` elements, where `K` is the
#'    number of study arms.
#'    For non-proportional models, a matrix with `K - 1` rows and `J - 1` columns,
#'    where `K` is the number of study arms and
#'    `J` is the number of study visits.
#'  * `gamma`: a vector with `V` elements, where `V` is the
#'    number of columns in the covariate adjustment model matrix `W`.
#'    If you are unsure of `V`, simply fit a test model
#'    (e.g. `fit <- pmrm_model_decline(...)`)
#'    and then check `ncol(fit$constants$W)`.
#'  * `phi`: a vector with the same length as `visit_times`
#'    (which may be different from the length of `spline_knots`).
#'  * `rho`: a vector with `J * (J - 1) / 2` elements,
#'     where `J` is the length of `visit_times`.
#'
#'   You can generate an example of the format of this list
#'   by fitting a test model (e.g. `fit <- pmrm_model_decline(...)`)
#'   and then extracting `fit$initial` or `fit$final`.
#' @param slowing `TRUE` to fit a slowing model,
#'   `FALSE` to fit a decline model.
#' @param proportional `TRUE` to fit a proportional model,
#'   `FALSE` to fit a non-proportional model.
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
