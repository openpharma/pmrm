# Confidence intervals of parameters

Compute confidence intervals of the family of model parameters specified
in `parm`.

## Usage

``` r
# S3 method for class 'pmrm_fit'
confint(object, parm = NULL, level = 0.95, ...)
```

## Arguments

- object:

  a fitted model object.

- parm:

  a specification of which parameters are to be given confidence
  intervals, either a vector of numbers or a vector of names. If
  missing, all parameters are considered.

- level:

  the confidence level required.

- ...:

  additional argument(s) for methods.

## Value

A numeric matrix with one row for each treatment effect parameter
(`theta`) and named columns with the lower and upper bounds of 2-sided
confidence intervals on the parameters.

## Details

See
[`vignette("models", package = "pmrm")`](https://wlandau.github.io/pmrm/articles/models.md)
for details.

## See also

Other model comparison:
[`AIC.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/AIC.pmrm_fit.md),
[`BIC.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/BIC.pmrm_fit.md),
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
  confint(fit)
#>           2.5 %    97.5 %
#> arm_2 0.0491933 0.2607419
#> arm_3 0.1190821 0.3252943
```
