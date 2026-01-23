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

  A fitted model object of class `"pmrm_fit"`.

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
[`vignette("models", package = "pmrm")`](https://openpharma.github.io/pmrm/articles/models.md)
for details.

## See also

Other estimates:
[`VarCorr.pmrm_fit()`](https://openpharma.github.io/pmrm/reference/VarCorr.pmrm_fit.md),
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
  coef(fit)
#>     arm_2     arm_3 
#> 0.1549676 0.2221882 
```
