# `pmrm` residuals.

Compute the residuals (responses minus fitted values) of a fitted
progression model for repeated measures.

## Usage

``` r
# S3 method for class 'pmrm_fit'
residuals(object, ..., data = object$data, adjust = TRUE)
```

## Arguments

- object:

  A fitted model object of class `"pmrm_fit"` produced by
  [`pmrm_model_decline()`](https://wlandau.github.io/pmrm/reference/pmrm_model_decline.md)
  or
  [`pmrm_model_slowing()`](https://wlandau.github.io/pmrm/reference/pmrm_model_slowing.md).

- ...:

  Not used.

- data:

  A `tibble` or data frame with one row per patient visit. This is the
  new data for making predictions. It must have all the same columns as
  the original you fit with the model, except that the outcome column
  can be entirely absent. `object$data` is an example dataset that will
  work. It is just like the original data, except that rows with missing
  responses are removed, and the remaining rows are sorted by patient ID
  and categorical scheduled visit.

- adjust:

  `TRUE` or `FALSE`. `adjust = TRUE` returns estimates and inference for
  covariate-adjusted `mu_ij` values (defined in
  [`vignette("models", package = "pmrm")`](https://wlandau.github.io/pmrm/articles/models.md))
  for new data. `adjust = FALSE` instead returns inference on
  `mu_ij - W %*% gamma`, the non-covariate-adjusted predictions useful
  in plotting a continuous disease progression trajectory in
  [`plot.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/plot.pmrm_fit.md).

## Value

A numeric vector of residuals corresponding to the rows of the data
supplied in the `data` argument.

## See also

Other estimates and predictions:
[`VarCorr.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/VarCorr.pmrm_fit.md),
[`coef.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/coef.pmrm_fit.md),
[`fitted.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/fitted.pmrm_fit.md),
[`plot.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/plot.pmrm_fit.md),
[`pmrm_estimates()`](https://wlandau.github.io/pmrm/reference/pmrm_estimates.md),
[`pmrm_marginals()`](https://wlandau.github.io/pmrm/reference/pmrm_marginals.md),
[`predict.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/predict.pmrm_fit.md),
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
  str(residuals(fit))
#>  num [1:1500] 1.242 -0.314 1.341 1.224 0.347 ...
```
