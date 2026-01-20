# Treatment effect parameters

Extract the `theta` parameter from a progression model for repeated
measures.

## Usage

``` r
# S3 method for class 'pmrm_fit'
coef(object, ...)
```

## Arguments

- object:

  A fitted model object of class `"pmrm_fit"` produced by
  [`pmrm_model_decline()`](https://wlandau.github.io/pmrm/reference/pmrm_model_decline.md)
  or
  [`pmrm_model_slowing()`](https://wlandau.github.io/pmrm/reference/pmrm_model_slowing.md).

- ...:

  Not used.

## Value

For proportional models, a named vector of `theta` estimates with one
element for each active study arm. For non-proportional models, a named
matrix of `theta` with one row for each active study arm and one column
for each post-baseline scheduled visit. Elements, rows, and columns are
named with arm/visit names as appropriate.

## Details

See
[`vignette("models", package = "pmrm")`](https://wlandau.github.io/pmrm/articles/models.md)
for details.

## See also

Other estimates and predictions:
[`VarCorr.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/VarCorr.pmrm_fit.md),
[`fitted.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/fitted.pmrm_fit.md),
[`plot.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/plot.pmrm_fit.md),
[`pmrm_estimates()`](https://wlandau.github.io/pmrm/reference/pmrm_estimates.md),
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
  coef(fit)
#>     arm_2     arm_3 
#> 0.1549676 0.2221882 
```
