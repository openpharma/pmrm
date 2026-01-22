# Akaike information criterion (AIC)

Extract the Akaike information criterion (AIC) of a progression model
for repeated measures (PMRM).

## Usage

``` r
# S3 method for class 'pmrm_fit'
AIC(object, ..., k = NULL)
```

## Arguments

- object:

  A fitted model object of class `"pmrm_fit"`.

- ...:

  Not used.

- k:

  Not used. Must be `NULL`.

## Value

Numeric scalar, the Akaike information criterion (AIC) of the fitted
model.

## See also

Other model comparison:
[`BIC.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/BIC.pmrm_fit.md),
[`confint.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/confint.pmrm_fit.md),
[`deviance.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/deviance.pmrm_fit.md),
[`glance.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/glance.pmrm_fit.md),
[`logLik.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/logLik.pmrm_fit.md),
[`summary.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/summary.pmrm_fit.md)

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
  AIC(fit)
#> [1] 4276.847
```
