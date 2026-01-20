#' @title Fit the proportional decline model.
#' @export
#' @family models
#' @description Fit the proportional decline model
#'   to a clinical dataset on a progressive disease.
#' @details Initial parameter values for the optimization
#'   are chosen with a simple
#'   back-of-the-envelope least-squares model.
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
#' @return A `pmrm` fit object of class `c("pmrm_fit_decline", "pmrm_fit")`.
#'   For details, see the "pmrm fit objects" section of this help file.
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
#'  * `theta`: a vector with `K - 1` elements, where `K` is the
#'    number of study arms.
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
#' @examples
#'   set.seed(0L)
#'   simulation <- pmrm_simulate_decline(
#'     visit_times = seq_len(5L) - 1,
#'     gamma = c(1, 2)
#'   )
#'   fit <- pmrm_model_decline(
#'     data = simulation,
#'     outcome = "y",
#'     time = "t",
#'     patient = "patient",
#'     visit = "visit",
#'     arm = "arm",
#'     covariates = ~ w_1 + w_2
#'   )
#'   str(fit$estimates)
#'   names(fit)
pmrm_model_decline <- function(
  data,
  outcome,
  time,
  patient,
  visit,
  arm,
  covariates = ~0,
  visit_times = NULL,
  spline_knots = visit_times,
  spline_method = c("natural", "fmm"),
  reml = FALSE,
  hessian = c("divergence", "never", "always"),
  saddle = FALSE,
  control = list(eval.max = 4000L, iter.max = 4000L),
  initial_method = c("regression", "regression_control", "zero"),
  initial = NULL,
  silent = TRUE
) {
  pmrm_model(
    data = data,
    outcome = outcome,
    time = time,
    patient = patient,
    visit = visit,
    arm = arm,
    covariates = covariates,
    visit_times = visit_times,
    spline_knots = spline_knots,
    spline_method = match.arg(spline_method),
    reml = reml,
    hessian = match.arg(hessian),
    saddle = saddle,
    control = control,
    initial_method = match.arg(initial_method),
    initial = initial,
    silent = silent,
    slowing = FALSE,
    proportional = TRUE
  )
}
