# Tidy a fitted PMRM.

Return tidy parameter summaries of a progression model for repeated
measures (PMRM).

## Usage

``` r
# S3 method for class 'pmrm_fit'
tidy(x, ...)
```

## Arguments

- x:

  A fitted progression model for repeated measures (PMRM).

- ...:

  Not used.

## Value

A tidy `tibble` with one row for each treatment effect model parameter
(`theta`) and columns with the parameter name (study arm and/or visit it
corresponds to), estimate, and standard error. This format aligns with
the [`tidy()`](https://generics.r-lib.org/reference/tidy.html) method of
similar fitted models in R.

## See also

Other estimates:
[`VarCorr.pmrm_fit()`](https://openpharma.github.io/pmrm/reference/VarCorr.pmrm_fit.md),
[`coef.pmrm_fit()`](https://openpharma.github.io/pmrm/reference/coef.pmrm_fit.md),
[`pmrm_marginals()`](https://openpharma.github.io/pmrm/reference/pmrm_marginals.md),
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
  tidy(fit)
#> # A tibble: 2 × 3
#>   term  estimate std.error
#>   <chr>    <dbl>     <dbl>
#> 1 arm_2    0.155    0.0540
#> 2 arm_3    0.222    0.0526
```
