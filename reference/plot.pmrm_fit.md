# Plot a fitted PMRM.

Plot a fitted progression model for repeated measures (PMRM) against the
data.

## Usage

``` r
# S3 method for class 'pmrm_fit'
plot(
  x,
  y = NULL,
  ...,
  confidence = 0.95,
  show_data = TRUE,
  show_marginals = TRUE,
  show_predictions = FALSE,
  facet = TRUE,
  alpha = 0.25
)
```

## Arguments

- x:

  A fitted model object of class `"pmrm_fit"` returned by a `pmrm`
  model-fitting function.

- y:

  Not used.

- ...:

  Not used.

- confidence:

  Numeric between 0 and 1, the confidence level to use in the 2-sided
  confidence intervals.

- show_data:

  `TRUE` to plot data-based visit-specific data means and confidence
  intervals as boxes. `FALSE` to omit.

- show_marginals:

  `TRUE` to plot model-based confidence intervals and estimates of
  marginal means as boxes and horizontal lines within those boxes,
  respectively. Uses
  [`pmrm_marginals()`](https://openpharma.github.io/pmrm/reference/pmrm_marginals.md)
  with the given level of confidence. `FALSE` to omit.

- show_predictions:

  `TRUE` to plot expected outcomes and confidence bands with lines and
  shaded regions, respectively. Uses
  [`predict.pmrm_fit()`](https://openpharma.github.io/pmrm/reference/predict.pmrm_fit.md)
  with `adjust = FALSE` and the given level of confidence on the
  original dataset used to fit the model. Predictions on a full dataset
  are generally slow, so the default is `FALSE`.

- facet:

  `TRUE` to facet the plot by study arm, `FALSE` to overlay everything
  in a single panel.

- alpha:

  Numeric between 0 and 1, opacity level of the model-based confidence
  bands.

## Value

A `ggplot` object with the plot.

## Details

The plot shows the following elements:

- Raw estimates and confidence intervals on the data, as boxes (if
  `show_data` is `TRUE`).

- Model-based estimates and confidence intervals as points and error
  bars, respectively (if `show_marginals` is `TRUE`).

- Continuous model-based estimates and confidence bands as lines and
  shaded regions, respectively (if `show_predictions` is `TRUE`).

## See also

Other visualization:
[`print.pmrm_fit()`](https://openpharma.github.io/pmrm/reference/print.pmrm_fit.md)

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
  plot(fit)
```
