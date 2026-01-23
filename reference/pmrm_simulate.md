# Simulate data.

Simulate data from a progression model for repeated measures.

## Usage

``` r
pmrm_simulate(
  patients,
  visit_times,
  spline_knots,
  spline_method,
  tau,
  alpha,
  beta,
  gamma,
  sigma,
  rho,
  slowing,
  proportional
)
```

## Arguments

- patients:

  Positive integer scalar, total number of patients in the output
  dataset. Patients are allocated (roughly) uniformly across the study
  arms.

- visit_times:

  Numeric vector, the continuous scheduled time after randomization of
  each study visit.

- spline_knots:

  Numeric vector of spline knots on the continuous scale, including
  boundary knots.

- spline_method:

  Character string, spline method to use for the base model. Must be
  `"natural"` or `"fmm"`. See
  [`stats::splinefun()`](https://rdrr.io/r/stats/splinefun.html) for
  details.

- tau:

  Positive numeric scalar, standard deviation for jittering the
  simulated time points. Defaults to 0 so that the observed continuous
  times are just the scheduled visit times.

- alpha:

  Numeric vector of spline coefficients for simulating the mean function
  `f(t_{ij} | spline_knots, alpha)`. Must have `length(spline_knots)`
  elements.

- beta:

  Treatment effect parameters. Input format and interpretation vary from
  model to model.

- gamma:

  Numeric vector of model coefficients for covariate adjustment. The
  simulation functions in `pmrm` simulate `length(gamma)` columns for
  the covariate adjustment model matrix `W`. Set to `numeric(0)` to omit
  covariates.

- sigma:

  A positive numeric vector of visit-level standard deviation
  parameters.

- rho:

  A finite numeric vector of correlation parameters. Must have length
  `J * (J - 1) / 2`, where `J` is `length(visit_times)`. The full
  covariance matrix `Sigma` is given by
  `diag(sigma) %*% RTMB::unstructured(length(sigma))$corr(rho) %*% diag(sigma)`.

## Value

A `tibble` with simulated clinical data (see the "Simulated data"
section).

## Simulated data

The datasets returned from the simulation functions have one row per
patient visit and the following columns which conform to the notation
from
[`vignette("models", package = "pmrm")`](https://openpharma.github.io/pmrm/articles/models.md):

- `patient`: Character vector of patient ID labels.

- `visit`: Ordered factor of clinical visits with labels included.
  `min(visit)` indicates the baseline visit.

- `arm`: Ordered factor of study arms with visits included. `min(arm)`
  indicates the control arm.

- `i`: integer ID of each patient.

- `j`: integer ID of each clinical visit. `j == 1` at baseline.

- `k`: integer ID of the study arm of patient `i`. `k == 1` for the
  control arm.

- `y`: clinical outcomes.

- `t`: observed continuous time since baseline.

- `beta`: the scalar component of the treatment effect parameter `beta`
  defined for patient `i`.

- `mu`: expected clinical outcome at the given patient visit.

- `w_*`: columns of the covariate adjustment model matrix `W`.

- `e`: residuals.
