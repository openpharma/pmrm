# Print a fitted PMRM.

Print a fitted progression model for repeated measures (PMRM).

## Usage

``` r
# S3 method for class 'pmrm_fit'
print(x, digits = 3L, ...)
```

## Arguments

- x:

  A fitted progression model for repeated measures (PMRM).

- digits:

  Non-negative integer, number of digits for rounding.

- ...:

  Not used.

## Value

`NULL` (invisibly). Called for side effects (printing to the R console).

## See also

Other visualization:
[`plot.pmrm_fit()`](https://openpharma.github.io/pmrm/reference/plot.pmrm_fit.md)

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
  print(fit)
#> Model:
#> 
#>   PMRM type:        decline
#>   Parameterization: proportional
#> 
#> Fit:
#> 
#>   Convergence:    converged
#>   Observations:   1500
#>   Parameters:     24
#>   Log likelihood: -2114.424
#>   Deviance:       4228.847
#>   AIC:            4276.847
#>   BIC:            4404.364
#> 
#> Treatment effects:
#> 
#>          estimate  std.error
#>   arm_2 0.1549676 0.05396748
#>   arm_3 0.2221882 0.05260613
```
