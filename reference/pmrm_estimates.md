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
  model-fitting function such as
  [`pmrm_model_decline()`](https://wlandau.github.io/pmrm/reference/pmrm_model_decline.md).

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
  simulation <- pmrm_simulate_decline(
    visit_times = seq_len(5L) - 1,
    gamma = c(1, 2)
  )
  fit <- pmrm_model_decline(
    data = simulation,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2
  )
  pmrm_estimates(fit, parameter = "beta")
#> # A tibble: 3 × 6
#>   parameter arm   estimate standard_error   lower  upper
#>   <chr>     <ord>    <dbl>          <dbl>   <dbl>  <dbl>
#> 1 beta      arm_1    0            NA      NA      NA    
#> 2 beta      arm_2    0.155         0.0540  0.0492  0.261
#> 3 beta      arm_3    0.222         0.0526  0.119   0.325
  pmrm_estimates(fit, parameter = "alpha")
#> # A tibble: 5 × 6
#>   parameter index estimate standard_error  lower upper
#>   <chr>     <int>    <dbl>          <dbl>  <dbl> <dbl>
#> 1 alpha         1 0.000599         0.0580 -0.113 0.114
#> 2 alpha         2 0.700            0.0683  0.566 0.834
#> 3 alpha         3 1.04             0.0732  0.893 1.18 
#> 4 alpha         4 1.39             0.0769  1.24  1.54 
#> 5 alpha         5 1.64             0.0834  1.48  1.81 
```
