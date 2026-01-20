# Treatment effect parameter covariance matrix

Extract the covariance matrix of the treatment effect parameters of a
progression model for repeated measures.

## Usage

``` r
# S3 method for class 'pmrm_fit'
vcov(object, ...)
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

A matrix with covariance of each pair of `theta` parameters. Rows and
columns are labeled (by just study arm for proportional models, arm and
visit for non-proportional models.)

## See also

Other estimates and predictions:
[`VarCorr.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/VarCorr.pmrm_fit.md),
[`coef.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/coef.pmrm_fit.md),
[`fitted.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/fitted.pmrm_fit.md),
[`plot.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/plot.pmrm_fit.md),
[`pmrm_estimates()`](https://wlandau.github.io/pmrm/reference/pmrm_estimates.md),
[`pmrm_marginals()`](https://wlandau.github.io/pmrm/reference/pmrm_marginals.md),
[`predict.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/predict.pmrm_fit.md),
[`residuals.pmrm_fit()`](https://wlandau.github.io/pmrm/reference/residuals.pmrm_fit.md)

## Examples

``` r
  set.seed(0L)
  simulation <- pmrm_simulate_slowing(
    visit_times = seq_len(5L) - 1,
    gamma = c(1, 2)
  )
  fit <- pmrm_model_slowing(
    data = simulation,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2
  )
  vcov(fit)
#>               arm_2:visit_2 arm_2:visit_3 arm_2:visit_4 arm_2:visit_5
#> arm_2:visit_2  2.162988e-02  -0.006151467  -0.002401060  6.481015e-05
#> arm_2:visit_3 -6.151467e-03   0.024403604   0.003697296  2.563572e-03
#> arm_2:visit_4 -2.401060e-03   0.003697296   0.005533995  1.801208e-03
#> arm_2:visit_5  6.481015e-05   0.002563572   0.001801208  4.369328e-03
#> arm_3:visit_2  7.790731e-03  -0.003979014  -0.001533088 -1.947778e-04
#> arm_3:visit_3 -2.769498e-02   0.026906926   0.009822227  2.856689e-03
#> arm_3:visit_4 -2.109110e-03   0.004426184   0.002338528  1.681363e-03
#> arm_3:visit_5 -2.531156e-04   0.001439367   0.001720711  2.056731e-03
#>               arm_3:visit_2 arm_3:visit_3 arm_3:visit_4 arm_3:visit_5
#> arm_2:visit_2  0.0077907308  -0.027694984  -0.002109110 -0.0002531156
#> arm_2:visit_3 -0.0039790145   0.026906926   0.004426184  0.0014393666
#> arm_2:visit_4 -0.0015330881   0.009822227   0.002338528  0.0017207111
#> arm_2:visit_5 -0.0001947778   0.002856689   0.001681363  0.0020567315
#> arm_3:visit_2  0.0182266869  -0.022780315  -0.002189579  0.0001137410
#> arm_3:visit_3 -0.0227803155   0.632872290   0.010976935  0.0024803578
#> arm_3:visit_4 -0.0021895787   0.010976935   0.005771546  0.0016207117
#> arm_3:visit_5  0.0001137410   0.002480358   0.001620712  0.0043726367
```
