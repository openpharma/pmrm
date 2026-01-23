# Estimated covariance matrix

Extract estimated covariance matrix among visits within patients.

## Usage

``` r
# S3 method for class 'pmrm_fit'
VarCorr(x, sigma = NA, ...)
```

## Arguments

- x:

  A fitted model object of class `"pmrm_fit"`.

- sigma:

  Not used for `pmrm`.

- ...:

  Not used.

## Value

A matrix `J` rows and `J` columns, where `J` is the number of scheduled
visits in the clinical trial.

## See also

Other estimates:
[`coef.pmrm_fit()`](https://openpharma.github.io/pmrm/reference/coef.pmrm_fit.md),
[`pmrm_marginals()`](https://openpharma.github.io/pmrm/reference/pmrm_marginals.md),
[`tidy.pmrm_fit()`](https://openpharma.github.io/pmrm/reference/tidy.pmrm_fit.md),
[`vcov.pmrm_fit()`](https://openpharma.github.io/pmrm/reference/vcov.pmrm_fit.md)

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
  VarCorr(fit)
#>              visit_1      visit_2     visit_3      visit_4      visit_5
#> visit_1  1.014197254 -0.005377564 -0.06587390  0.029608749 -0.100061354
#> visit_2 -0.005377564  0.944702148 -0.08021265 -0.073493333  0.056003869
#> visit_3 -0.065873896 -0.080212654  1.04399327 -0.045969231  0.148149051
#> visit_4  0.029608749 -0.073493333 -0.04596923  0.946986875  0.001358265
#> visit_5 -0.100061354  0.056003869  0.14814905  0.001358265  1.015609461
```
