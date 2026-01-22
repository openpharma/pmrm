# Marginal means

Report the estimates and standard errors of marginal means at each study
arm and visit. The assumed visit times should have been given in the
`marginals` argument of the model-fitting function. Use the `type`
argument to choose marginal means of the outcomes, marginal estimates of
change from baseline, and marginal estimates of treatment effects.

## Usage

``` r
pmrm_marginals(fit, type = c("outcome", "change", "effect"), confidence = 0.95)
```

## Arguments

- fit:

  A `pmrm` fitted model object returned by a model-fitting function.

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
standard error, 2-sided confidence bounds, and indicator columns. Some
estimates, standard errors, and confidence bounds may be `NA_real_` if
they correspond to the reference level subtracted out in
change-from-baseline or treatment effect calculations.

## See also

Other estimates:
[`VarCorr.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/VarCorr.pmrm_fit.md),
[`coef.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/coef.pmrm_fit.md),
[`plot.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/plot.pmrm_fit.md),
[`tidy.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/tidy.pmrm_fit.md),
[`vcov.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/vcov.pmrm_fit.md)

## Examples

``` r
  set.seed(0L)
  simulation <- pmrm_simulate_decline_proportional(
    visit_times = seq_len(5L) - 1,
    gamma = c(1, 2)
  )
  fit <- pmrm_model_decline_proportional(
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
#>  1 arm_1 visit_1     0 0.000599         0.0580 -0.113 0.114
#>  2 arm_1 visit_2     1 0.700            0.0683  0.566 0.834
#>  3 arm_1 visit_3     2 1.04             0.0732  0.893 1.18 
#>  4 arm_1 visit_4     3 1.39             0.0769  1.24  1.54 
#>  5 arm_1 visit_5     4 1.64             0.0834  1.48  1.81 
#>  6 arm_2 visit_1     0 0.000599         0.0580 -0.113 0.114
#>  7 arm_2 visit_2     1 0.591            0.0586  0.477 0.706
#>  8 arm_2 visit_3     2 0.876            0.0679  0.743 1.01 
#>  9 arm_2 visit_4     3 1.17             0.0716  1.03  1.31 
#> 10 arm_2 visit_5     4 1.39             0.0786  1.23  1.54 
#> 11 arm_3 visit_1     0 0.000599         0.0580 -0.113 0.114
#> 12 arm_3 visit_2     1 0.544            0.0549  0.437 0.652
#> 13 arm_3 visit_3     2 0.807            0.0639  0.681 0.932
#> 14 arm_3 visit_4     3 1.08             0.0695  0.942 1.21 
#> 15 arm_3 visit_5     4 1.28             0.0775  1.13  1.43 
```
