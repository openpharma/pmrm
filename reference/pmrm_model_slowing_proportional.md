# Fit the proportional slowing model.

Fit the proportional slowing model to a clinical dataset on a
progressive disease.

## Usage

``` r
pmrm_model_slowing_proportional(
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
)
```

## Arguments

- data:

  A data frame or `tibble` of clinical data.

- outcome:

  Character string, name of the column in the data with the numeric
  outcome variable on the continuous scale. Could be a clinical measure
  of healthy or of disease severity. Baseline is part of the model, so
  the `outcome` should not already be a change from baseline. The vector
  of outcomes may have missing values, either with explicit `NA`s or
  with rows in the data missing for one or more visits.

- time:

  Character string, name of the column in the data with the numeric time
  variable on the continuous scale. This time is the time since
  enrollment/randomization of each patient. A time value of 0 should
  indicate baseline.

- patient:

  Character string, name of the column in the data with the patient ID.
  This vector could be a numeric, integer, factor, or character vector.
  `pmrm` automatically converts it into an unordered factor.

- visit:

  Character string, name of the column in the data which indicates the
  study visit of each row. This column could be a numeric, integer,
  factor, or character vector. An ordered factor is highly recommended
  because `pmrm` with levels assumed to be in chronological order. The
  minimum visit must be baseline.

- arm:

  Character string, name of the column in the data which indicates the
  study arm of each row. This column could be a numeric, integer,
  factor, or character vector. An ordered factor is highly recommended
  because `pmrm` automatically converts `data[[arm]]` into an ordered
  factor anyway. The minimum level is assumed to be the control arm.

- covariates:

  Partial right-sided formula of concomitant terms in the model for
  covariate adjustment (e.g. by age, gender, biomarker status, etc.).
  Should not include main variables such as the values of `outcome`,
  `time`, `patient`, `visit`, or `arm`. The columns in the data
  referenced in the formula must not have any missing values.

  Set `covariates` to `~ 0` (default) to opt out of covariate
  adjustment. The intercept term is removed from the model matrix `W`
  whether or not the formula begins with \`~ 0.

- visit_times:

  Numeric vector, the continuous scheduled time of each study visit
  (since randomization). If `NULL`, each visit time is automatically set
  set to the median of the observed times at categorical visit in the
  data.

- spline_knots:

  Numeric vector of spline knots on the continuous scale, including
  boundary knots.

- spline_method:

  Character string, spline method to use for the base model. Must be
  `"natural"` or `"fmm"`. See
  [`stats::splinefun()`](https://rdrr.io/r/stats/splinefun.html) for
  details.

- reml:

  `TRUE` to fit the model with restricted maximum likelihood (REML),
  which involves integrating out fixed effects. `FALSE` to use
  unrestricted maximum likelihood. If `reml` is `TRUE`, then `hessian`
  is automatically set to `"never"`.

- hessian:

  Character string controlling when to supply the Hessian matrix of the
  objective function to the optimizer
  [`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html). Supplying
  the Hessian usually slows down optimization but may improve
  convergence in some cases, particularly saddle points in the objective
  function.

  The `hessian` argument is automatically set to `"never"` whenever
  `reml` is `TRUE`.

  The `hessian` argument must be one of the following values:

  - `"divergence"`: first try the model without supplying the Hessian.
    Then if the model does not converge, retry while supplying the
    Hessian.

  - `"never"`: fit the model only once and do not supply the Hessian to
    [`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html).

  - `"always"`: fit the model once and supply the Hessian to
    [`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html).

- saddle:

  `TRUE` to check if the optimization hit a saddle point, and if it did,
  treat the model fit as if it diverged. `FALSE` to skip this check for
  the sake of speed.

- control:

  A named list of control parameters passed directly to the `control`
  argument of [`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html).

- initial_method:

  Character string, name of the method for computing initial values.
  Ignored unless `initial` is `NULL`. Must have one of the following
  values:

  - `"regression"`: sets the spline vertical distances `alpha` to the
    fitted values at the knots of a simple linear regression of the
    responses versus continuous time. Sets all the other true model
    parameters to 0.

  - `"regression_control"`: like `"regression"` except we only use the
    data from the control group. Sets all the other true model
    parameters to 0.

  - `"zero"`: sets all true model parameters to 0, including `alpha`.

- initial:

  If `initial` is a named list, then `pmrm` uses this list as the
  initial parameter values for the optimization. Otherwise, `pmrm`
  automatically computes the starting values using the method given in
  the `initial_method` argument (see below).

  If `initial` is a list, then it must have the following named finite
  numeric elements conforming to all the true parameters defined in
  [`vignette("models", package = "pmrm")`](https://wlandau.github.io/pmrm/articles/models.md):

  - `alpha`: a vector with the same length as `spline_knots`.

  - `theta`: a vector with `K - 1` elements, where `K` is the number of
    study arms.

  - `gamma`: a vector with `V` elements, where `V` is the number of
    columns in the covariate adjustment model matrix `W`. If you are
    unsure of `V`, simply fit a test model (e.g.
    `fit <- pmrm_model_slowing_proportional(...)`) and then check
    `ncol(fit$constants$W)`.

  - `phi`: a vector with the same length as `visit_times` (which may be
    different from the length of `spline_knots`).

  - `rho`: a vector with `J * (J - 1) / 2` elements, where `J` is the
    length of `visit_times`.

  You can generate an example of the format of this list by fitting a
  test model (e.g. `fit <- pmrm_model_slowing_proportional(...)`) and
  then extracting `fit$initial` or `fit$final`.

- silent:

  As [MakeADFun](https://rdrr.io/pkg/TMB/man/MakeADFun.html).

## Value

A `pmrm` fit object of class `c("pmrm_fit_slowing", "pmrm_fit")`. For
details, see the "pmrm fit objects" section of this help file.

## Details

See
[`vignette("models", package = "pmrm")`](https://wlandau.github.io/pmrm/articles/models.md)
for details.

## pmrm fit objects

A `"pmrm_fit"` object is a classed list returned by modeling functions.
It has the following named elements:

- `data`: a `tibble`, the input data with the missing outcomes removed
  and the remaining rows sorted by patient and visit within patient. The
  data has a special `"pmrm_data"` class and should not be modified by
  the user.

- `constants`: a list of fixed quantities from the data that the
  objective function uses in the optimization. Most of these quantities
  are defined in the modeling and simulation vignettes in the `pmrm`
  package. `n_visits` is a positive integer vector with the number of
  non-missing outcomes for each patient.

- `options`: a list of low-level model-fitting options for `RTMB`.

- `objective`: the objective function for the optimization. Returns the
  minus log likelihood of the model. The arguments are (1) a list of
  constants, and (2) a list of model parameters. Both arguments have
  strict formatting requirements. For (1), see the `constants` element
  of the fitted model object. For (2), see `initial` or `final`.
  `model$fn` (from the `model` element of the fitted model object)
  contains a copy of the objective function that only takes a parameter
  list. (The constants are in the closure of `model$fn`.)

- `model`: model object returned by
  [`RTMB::MakeADFun()`](https://rdrr.io/pkg/RTMB/man/TMB-interface.html)
  with the compiled objective function and gradient. The elements can be
  supplied to an optimization routine in R such as
  [`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html).

- `optimization`: the object returned by
  [`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html) to perform
  the optimization that estimates the parameters.
  `optimization$convergence` equals 0 if an only if the model converges.

- `report`: object returned by
  [`RTMB::sdreport()`](https://rdrr.io/pkg/RTMB/man/TMB-interface.html)
  which has information on the standard deviations of model parameters.

- `initial`: a list of model parameters initial values. Includes true
  parameters like `theta` and `alpha` but does not include derived
  parameters like `beta` or `sigma`. You can supply your own list of
  similarly formatted initial values to the `initial` argument of the
  modeling function you choose.

- `final`: a list of model parameter estimates after optimization, but
  not including derived parameters like `beta` or `sigma`. The format is
  exactly the same as `initial` (see above) to help deal with divergent
  model fits. If your model fit diverged and you want to try resume the
  optimization with slightly better values, you can modify values in
  `final` and supply the result to the `initial` argument of the
  modeling function.

- `estimates`: a full list of parameter estimates, including derived
  parameters

- `standard_errors`: a list of parameter standard errors.

- `metrics`: a list of high-level model metrics, including:

  - `n_observations`: positive integer scalar, number of non-missing
    observations in the data.

  - `n_parameters`: positive integer scalar, number of model parameters
    in the data. Includes true parameters like `theta` but excludes
    downstream functions of parameters such as `beta`.

  - `log_likelihood`: numeric scalar, the maximized log likelihood of
    the fitted model.

  - `deviance`: deviance of the fitted model, defined here as
    `-2 * log_likelihood`.

  - `aic`: numeric scalar, the Akaike information criterion of the
    fitted model.

  - `bic`: numeric scalar, the Bayesian information criterion of the
    fitted model.

- `spline`: a vectorized function that accepts continuous time `x` and
  returns the value of the fitted spline `f(x | spline_knots, alpha)` at
  time `x` given the user-specified knots `spline_knots` and the maximum
  likelihood estimates of `alpha`. Useful for diagnosing strange
  behavior in the fitted spline. If the spline behaves oddly, especially
  extrapolating beyond the range of the time points, please consider
  adjusting the knots `spline_knots` or the initial values of `alpha`
  when refitting the model.

## See also

Other models:
[`pmrm_model_decline_nonproportional()`](https://wlandau.github.io/pmrm/reference/pmrm_model_decline_nonproportional.md),
[`pmrm_model_decline_proportional()`](https://wlandau.github.io/pmrm/reference/pmrm_model_decline_proportional.md),
[`pmrm_model_slowing_nonproportional()`](https://wlandau.github.io/pmrm/reference/pmrm_model_slowing_nonproportional.md)

## Examples

``` r
  set.seed(0L)
  simulation <- pmrm_simulate_slowing_proportional(
    visit_times = seq_len(5L) - 1,
    gamma = c(1, 2)
  )
  fit <- pmrm_model_slowing_proportional(
    data = simulation,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2
  )
  str(fit$estimates)
#> List of 9
#>  $ alpha : Named num [1:5] -0.00671 0.69894 1.05246 1.38003 1.66483
#>   ..- attr(*, "names")= chr [1:5] "1" "2" "3" "4" ...
#>  $ theta : num [1:2] 0.179 0.218
#>  $ gamma : num [1:2] 1.01 1.96
#>  $ phi   : num [1:5] 0.00707 -0.02825 0.02112 -0.02688 0.0077
#>  $ rho   : num [1:10] -0.0062 -0.0648 0.0306 -0.1 -0.0806 ...
#>  $ beta  : num [1:3] 0 0.179 0.218
#>  $ sigma : num [1:5] 1.007 0.972 1.021 0.973 1.008
#>  $ Lambda: num [1:5, 1:5] 1 -0.0062 -0.0645 0.0305 -0.0983 ...
#>  $ Sigma : num [1:5, 1:5] 1.01424 -0.00607 -0.06633 0.02987 -0.09972 ...
  names(fit)
#>  [1] "data"            "constants"       "options"         "objective"      
#>  [5] "model"           "optimization"    "report"          "initial"        
#>  [9] "final"           "estimates"       "standard_errors" "metrics"        
#> [13] "spline"         
```
