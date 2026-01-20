# Simulate proportional decline model.

Simulate a dataset from the proportional decline model.

## Usage

``` r
pmrm_simulate_decline(
  patients = 300,
  visit_times = seq(from = 0, to = 4, by = 1),
  spline_knots = visit_times,
  spline_method = c("natural", "fmm"),
  tau = 0,
  alpha = log(spline_knots + 1),
  beta = c(0, 0.1, 0.2),
  gamma = numeric(0L),
  sigma = rep(1, length(visit_times)),
  rho = rep(0, length(visit_times) * (length(visit_times) - 1L)/2L)
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

  Numeric vector with one element per study arm (including the control
  arm). Each element is the reduction in proportional decline of the
  corresponding arm relative to the control arm. The first element
  corresponds to the control arm and therefore must be 0.

- gamma:

  Numeric vector of model coefficients for covariate adjustment.
  `pmrm_simulate_decline()` simulates `length(gamma)` columns for the
  covariate adjustment model matrix `W`. Set to `numeric(0)` to omit
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

A `tibble` of clinical data simulated from the proportional decline
model. See the "Simulated data" section of this help file for details.

## Details

See
[`vignette("models", package = "pmrm")`](https://wlandau.github.io/pmrm/articles/models.md)
for details.

## Simulated data

The datasets returned from the simulation functions have one row per
patient visit and the following columns which conform to the notation
from
[`vignette("models", package = "pmrm")`](https://wlandau.github.io/pmrm/articles/models.md):

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

## See also

Other simulations:
[`pmrm_simulate_slowing()`](https://wlandau.github.io/pmrm/reference/pmrm_simulate_slowing.md)

## Examples

``` r
  pmrm_simulate_decline()
#> # A tibble: 1,500 × 11
#>    patient   visit   arm       i     j     k      y     t  beta    mu       e
#>    <chr>     <ord>   <ord> <int> <int> <int>  <dbl> <dbl> <dbl> <dbl>   <dbl>
#>  1 patient_1 visit_1 arm_1     1     1     1  2.72      0   0   0      2.72  
#>  2 patient_1 visit_2 arm_1     1     2     1 -0.379     1   0   0.693 -1.07  
#>  3 patient_1 visit_3 arm_1     1     3     1  1.72      2   0   1.10   0.621 
#>  4 patient_1 visit_4 arm_1     1     4     1  1.97      3   0   1.39   0.581 
#>  5 patient_1 visit_5 arm_1     1     5     1  0.668     4   0   1.61  -0.942 
#>  6 patient_2 visit_1 arm_2     2     1     2  0.644     0   0.1 0      0.644 
#>  7 patient_2 visit_2 arm_2     2     2     2  1.21      1   0.1 0.624  0.589 
#>  8 patient_2 visit_3 arm_2     2     3     2  0.438     2   0.1 0.989 -0.550 
#>  9 patient_2 visit_4 arm_2     2     4     2  1.22      3   0.1 1.25  -0.0277
#> 10 patient_2 visit_5 arm_2     2     5     2  0.366     4   0.1 1.45  -1.08  
#> # ℹ 1,490 more rows
```
