# Predict new outcomes

Return the expected values, standard errors, and confidence intervals of
new outcomes.

## Usage

``` r
# S3 method for class 'pmrm_fit'
predict(object, data = object$data, adjust = TRUE, confidence = 0.95, ...)
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

- confidence:

  Numeric between 0 and 1, the confidence level to use in the 2-sided
  confidence intervals.

- ...:

  Not used.

## Value

A `tibble` with one row for each row in the `data` argument and columns
`"estimate"`, `"standard_error"`, `"lower"`, and `"upper"`. Columns
`"lower"` and `"upper"` are lower and upper bounds of 2-sided confidence
intervals on the means. (The confidence intervals are not actually truly
prediction intervals because they do not include variability from
residuals.)

## See also

Other predictions:
[`fitted.pmrm_fit()`](https://openpharma.github.io/pmrm/reference/fitted.pmrm_fit.md),
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
  new_data <- pmrm_simulate_decline_proportional(
    patients = 1,
    visit_times = seq_len(5L) - 1,
    gamma = c(1, 2)
  )
  new_data$y <- NULL # Permitted but not strictly necessary.
  predict(fit, new_data)
#> Error in predict.pmrm_fit(fit, new_data): object 'fit' not found
```
