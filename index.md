# pmrm

[![CRAN](https://www.r-pkg.org/badges/version/pmrm)](https://CRAN.R-project.org/package=pmrm)
[![status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![check](https://github.com/openpharma/pmrm/actions/workflows/check.yaml/badge.svg)](https://github.com/openpharma/pmrm/actions?query=workflow%3Acheck)
[![codecov](https://codecov.io/gh/openpharma/pmrm/branch/main/graph/badge.svg?token=3T5DlLwUVl)](https://app.codecov.io/gh/openpharma/pmrm)

A progression model for repeated measures (PMRM) is a continuous-time
nonlinear mixed-effects model for longitudinal clinical trials in
progressive diseases. Unlike mixed models for repeated measures (MMRMs),
which estimate treatment effects as linear combinations of additive
effects on the outcome scale, PMRMs characterize treatment effects in
terms of the underlying disease trajectory. This framing yields
clinically interpretable quantities such as average time saved and
percent reduction in decline due to treatment. The pmrm package
implements the frequentist PMRM framework of Raket (2022) using
automatic differentiation via [RTMB](https://github.com/kaskr/RTMB)
(Kristensen et al. 2016).

# Documentation

Please see <https://openpharma.github.io/pmrm/> for model definitions, a
usage tutorial, and a function reference.

# Installation

There are multiple ways to install `pmrm`.

| Type        | Source | Command                                        |
|-------------|--------|------------------------------------------------|
| Release     | CRAN   | `install.packages("pmrm")`                     |
| Release     | GitHub | `pak::pkg_install("openpharma/pmrm@*release")` |
| Development | GitHub | `pak::pkg_install("openpharma/pmrm")`          |

# Citation

``` R
To cite pmrm in publications, please use:

  Landau WM, Raket LL, Kristensen K (2026). "Progression Models for
  Repeated Measures." R package, <https://openpharma.github.io/pmrm/>.

A BibTeX entry for LaTeX users is

  @Misc{,
    author = {William Michael Landau and Lars Lau Raket and Kasper Kristensen},
    title = {Progression Models for Repeated Measures},
    year = {2026},
    note = {R package},
    url = {https://openpharma.github.io/pmrm/},
  }

Please also cite the underlying methods paper:

  Raket, Lars Lau (2022). Progression Models for Repeated Measures:
  Estimating Novel Treatment Effects in Progressive Diseases.
  Statistics in Medicine, 41(28), 5537–57,
  https://doi.org/10.1002/sim.9581.

A BibTeX entry for LaTeX users is

  @Article{,
    author = {Lars Lau Raket},
    title = {Progression models for repeated measures: Estimating novel treatment effects in progressive diseases},
    journal = {Statistics in {M}edicine},
    year = {2022},
    volume = {41},
    number = {28},
    pages = {5537-5557},
    doi = {10.1002/sim.9581},
    url = {https://doi.org/10.1002/sim.9581},
  }
```

# References

Kristensen, Kasper, Anders Nielsen, Casper W. Berg, Hans Skaug, and
Bradley M. Bell. 2016. “TMB: Automatic Differentiation and Laplace
Approximation.” *Journal of Statistical Software* 70 (5): 1–21.
<https://doi.org/10.18637/jss.v070.i05>.

Raket, Lars Lau. 2022. “Progression Models for Repeated Measures:
Estimating Novel Treatment Effects in Progressive Diseases.” *Statistics
in Medicine* 41 (28): 5537–57. <https://doi.org/10.1002/sim.9581>.
