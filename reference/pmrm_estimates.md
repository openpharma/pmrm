# Parameter estimates and confidence intervals

Report parameter estimates and confidence intervals for a progression
model for repeated measures (PMRM).

## Usage

``` r
pmrm_estimates(
  fit,
  parameter = c("theta", "beta", "alpha", "gamma", "sigma", "phi", "rho", "Sigma",
    "Lambda"),
  confidence = 0.95
)
```

## Arguments

- fit:

  A fitted model object of class `"pmrm_fit"` returned by a `pmrm`
  model-fitting function.

- parameter:

  Character string, name of the type of model parameter to summarize.
  Must be one of `"beta"`, `"theta"`, `"alpha"`, `"gamma"`, `"sigma"`,
  `"rho"`, `"Sigma"`, or `"Lambda"`.

- confidence:

  Numeric between 0 and 1, the confidence level to use in 2-sided normal
  confidence intervals.

## Value

A `tibble` with one row for each scalar element of the selected model
parameter and columns with estimates, standard errors, lower and upper
bounds of two-sided normal confidence intervals, and indexing variables.
If applicable, the indexing variables are `arm` and/or `visit` to
indicate the study arm and study visit. If there is no obvious indexing
factor in the data, then a generic integer `index` column is used. For
covariance matrices, elements are identified with the `visit_row` and
`visit_column` columns.

`beta` is not a true parameter. Instead, it is a function of `theta` and
fixed at zero for the control arm and at baseline. At these marginals,
the standard errors and confidence intervals for `beta` are `NA_real_`.

## See also

Other estimates and predictions:
[`VarCorr.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/VarCorr.pmrm_fit.md),
[`coef.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/coef.pmrm_fit.md),
[`fitted.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/fitted.pmrm_fit.md),
[`plot.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/plot.pmrm_fit.md),
[`pmrm_marginals()`](https://wlandau.github.io/pmrm/reference/pmrm_marginals.md),
[`predict.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/predict.pmrm_fit.md),
[`residuals.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/residuals.pmrm_fit.md),
[`vcov.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/vcov.pmrm_fit.md)

## Examples

``` r
  set.seed(0L)
  simulation <- pmrm_simulate_decline_nonproportional(
    visit_times = seq_len(5L) - 1,
    gamma = c(1, 2)
  )
  fit <- pmrm_model_decline_nonproportional(
    data = simulation,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2
  )
  pmrm_estimates(fit, parameter = "beta")
#> # A tibble: 15 × 7
#>    parameter arm   visit   estimate standard_error   lower  upper
#>    <chr>     <ord> <ord>      <dbl>          <dbl>   <dbl>  <dbl>
#>  1 beta      arm_1 visit_1   0             NA      NA      NA    
#>  2 beta      arm_1 visit_2   0             NA      NA      NA    
#>  3 beta      arm_1 visit_3   0             NA      NA      NA    
#>  4 beta      arm_1 visit_4   0             NA      NA      NA    
#>  5 beta      arm_1 visit_5   0             NA      NA      NA    
#>  6 beta      arm_2 visit_1   0             NA      NA      NA    
#>  7 beta      arm_2 visit_2   0.431          0.141   0.154   0.708
#>  8 beta      arm_2 visit_3   0.0280         0.156  -0.278   0.334
#>  9 beta      arm_2 visit_4   0.260          0.0887  0.0865  0.434
#> 10 beta      arm_2 visit_5   0.281          0.0755  0.133   0.428
#> 11 beta      arm_3 visit_1   0             NA      NA      NA    
#> 12 beta      arm_3 visit_2   0.514          0.138   0.242   0.785
#> 13 beta      arm_3 visit_3   0.197          0.144  -0.0860  0.480
#> 14 beta      arm_3 visit_4   0.315          0.0867  0.145   0.485
#> 15 beta      arm_3 visit_5   0.319          0.0744  0.173   0.465
  pmrm_estimates(fit, parameter = "alpha")
#> # A tibble: 5 × 6
#>   parameter index estimate standard_error  lower upper
#>   <chr>     <int>    <dbl>          <dbl>  <dbl> <dbl>
#> 1 alpha         1  0.00245         0.0582 -0.112 0.117
#> 2 alpha         2  0.811           0.0968  0.622 1.00 
#> 3 alpha         3  0.910           0.102   0.711 1.11 
#> 4 alpha         4  1.39            0.0975  1.19  1.58 
#> 5 alpha         5  1.66            0.101   1.46  1.86 
```
