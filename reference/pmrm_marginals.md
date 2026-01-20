# Marginal means

Report the estimates and standard errors of marginal means at each study
arm and visit. The assumed visit times should have been given in the
`marginals` argument of the model-fitting function, e.g.
[`pmrm_model_decline()`](https://wlandau.github.io/pmrm/reference/pmrm_model_decline.md).
Use the `type` argument to choose marginal means of the outcomes,
marginal estimates of change from baseline, and marginal estimates of
treatment effects.

## Usage

``` r
pmrm_marginals(fit, type = c("outcome", "change", "effect"), confidence = 0.95)
```

## Arguments

- fit:

  A `pmrm` fitted model object returned by a model-fitting function such
  as
  [`pmrm_model_decline()`](https://wlandau.github.io/pmrm/reference/pmrm_model_decline.md).

- type:

  Character string. `"outcome"` reports marginal means on the outcome
  scale, `"change"` reports estimates of change from baseline, and
  `"effect"` reports estimates of treatment effects (change from
  baseline of each active arm minus that of the control arm.)

- confidence:

  A numeric from 0 to 1 with the confidence level for confidence
  intervals.

## Value

A `tibble` with one row per marginal mean and columns with the estimate,
standard error, 2-sided confidence bounds, and indicator columns.

## See also

Other estimates and predictions:
[`VarCorr.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/VarCorr.pmrm_fit.md),
[`coef.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/coef.pmrm_fit.md),
[`fitted.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/fitted.pmrm_fit.md),
[`plot.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/plot.pmrm_fit.md),
[`pmrm_estimates()`](https://wlandau.github.io/pmrm/reference/pmrm_estimates.md),
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
  fit <- pmrm_model_slowing(
    data = simulation,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2
  )
  pmrm_marginals(fit)
#> # A tibble: 15 × 7
#>    arm   visit    time estimate standard_error  lower upper
#>    <ord> <ord>   <dbl>    <dbl>          <dbl>  <dbl> <dbl>
#>  1 arm_1 visit_1     0  0.00279         0.0582 -0.111 0.117
#>  2 arm_1 visit_2     1  0.806           0.102   0.605 1.01 
#>  3 arm_1 visit_3     2  0.893           0.0859  0.725 1.06 
#>  4 arm_1 visit_4     3  1.39            0.0967  1.20  1.58 
#>  5 arm_1 visit_5     4  1.66            0.0999  1.46  1.85 
#>  6 arm_2 visit_1     0  0.00279         0.0582 -0.111 0.117
#>  7 arm_2 visit_2     1  0.806           0.102   0.605 1.01 
#>  8 arm_2 visit_3     2  0.893           0.0859  0.725 1.06 
#>  9 arm_2 visit_4     3  1.39            0.0967  1.20  1.58 
#> 10 arm_2 visit_5     4  1.66            0.0999  1.46  1.85 
#> 11 arm_3 visit_1     0  0.00279         0.0582 -0.111 0.117
#> 12 arm_3 visit_2     1  0.806           0.102   0.605 1.01 
#> 13 arm_3 visit_3     2  0.893           0.0859  0.725 1.06 
#> 14 arm_3 visit_4     3  1.39            0.0967  1.20  1.58 
#> 15 arm_3 visit_5     4  1.66            0.0999  1.46  1.85 
```
