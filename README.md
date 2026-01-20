
# pmrm <a href="https://wlandau.github.io/pmrm/"><img src="man/figures/logo.png" align="right" height="139"/></a>

A progression model for repeated measures (PMRM) is a longitudinal
continuous-time nonlinear model of a progressive disease. Some PMRMs are
designed to estimate the treatment effect on the time scale, which is
often more appropriate and more intuitive than the more typical
treatment effect on the outcome scale. This package implements
frequentist PMRMs by Raket (2022) using
[TMB](https://github.com/kaskr/adcomp) by Kristensen et al. (2016).

# Installation

You can install `pmrm` from GitHub.

``` r
pak::pkg_install("wlandau/pmrm")
```

# Citation

``` r
citation("pmrm")
#> To cite package 'pmrm' in publications use:
#> 
#>   Landau WM, Raket LL, Kristensen K (????). _pmrm: Progression Models
#>   for Repeated Measures_. R package version 0.0.1,
#>   <https://github.com/wlandau/pmrm>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {pmrm: Progression Models for Repeated Measures},
#>     author = {William Michael Landau and Lars Lau Raket and Kasper Kristensen},
#>     note = {R package version 0.0.1},
#>     url = {https://github.com/wlandau/pmrm},
#>   }
```

# References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-kristensen2016" class="csl-entry">

Kristensen, Kasper, Anders Nielsen, Casper W. Berg, Hans Skaug, and
Bradley M. Bell. 2016. “TMB: Automatic Differentiation and Laplace
Approximation.” *Journal of Statistical Software* 70 (5): 1–21.
<https://doi.org/10.18637/jss.v070.i05>.

</div>

<div id="ref-raket2022" class="csl-entry">

Raket, Lars Lau. 2022. “Progression Models for Repeated Measures:
Estimating Novel Treatment Effects in Progressive Diseases.” *Statistics
in Medicine* 41 (28): 5537–57. <https://doi.org/10.1002/sim.9581>.

</div>

</div>
