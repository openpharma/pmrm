# Changelog

## pmrm 0.0.4

CRAN release: 2026-04-03

- CRITICAL: estimate the proper marginal means for
  [`pmrm_marginals()`](https://openpharma.github.io/pmrm/reference/pmrm_marginals.md).
  `pmrm_objective()` previously reported the wrong ones for the slowing
  and non-proportional decline models.
- Remove unused `data` argument in
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html).
- Expand
  [`pmrm_estimates()`](https://openpharma.github.io/pmrm/reference/pmrm_estimates.md)
  testing.

## pmrm 0.0.3

CRAN release: 2026-03-12

- Use all of
  [`pmrm_data()`](https://openpharma.github.io/pmrm/reference/pmrm_data.md)
  and rebuild all ordered factors in
  [`predict()`](https://rdrr.io/r/stats/predict.html)
  ([\#5](https://github.com/openpharma/pmrm/issues/5),
  [@lbenz-lilly](https://github.com/lbenz-lilly)).
- Align `W` column mean centering between models and
  [`predict()`](https://rdrr.io/r/stats/predict.html)
  ([\#7](https://github.com/openpharma/pmrm/issues/7),
  [@lbenz-lilly](https://github.com/lbenz-lilly)).
- Directly check for missing values in covariate columns
  ([\#9](https://github.com/openpharma/pmrm/issues/9),
  [@lbenz-lilly](https://github.com/lbenz-lilly)).

## pmrm 0.0.2

CRAN release: 2026-01-30

- Fix citation and links for CRAN.

## pmrm 0.0.1

- First version.
