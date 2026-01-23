# Fitted values

Compute the fitted values of a fitted progression model for repeated
measures.

## Usage

``` r
# S3 method for class 'pmrm_fit'
fitted(object, data = object$data, adjust = TRUE, ...)
```

## Arguments

- object:

  A fitted model object of class `"pmrm_fit"`.

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
  [`vignette("models", package = "pmrm")`](https://openpharma.github.io/pmrm/articles/models.md))
  for new data. `adjust = FALSE` instead returns inference on
  `mu_ij - W %*% gamma`, the non-covariate-adjusted predictions useful
  in plotting a continuous disease progression trajectory in
  [`plot.pmrm_fit()`](https://openpharma.github.io/pmrm/reference/plot.pmrm_fit.md).

- ...:

  Not used.

## Value

A numeric vector of fitted values corresponding to the rows of the data
supplied in the `data` argument.

## Details

For `pmrm`, [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) is
much faster than [`predict()`](https://rdrr.io/r/stats/predict.html) for
large datasets, but the output only includes the estimates (no measures
of uncertainty).

## See also

Other predictions:
[`predict.pmrm_fit()`](https://openpharma.github.io/pmrm/reference/predict.pmrm_fit.md),
[`residuals.pmrm_fit()`](https://openpharma.github.io/pmrm/reference/residuals.pmrm_fit.md)

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
  str(fitted(fit))
#>  num [1:1500] -1.026 1.574 -2.106 0.727 -0.773 ...
```
