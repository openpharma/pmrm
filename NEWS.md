# pmrm development version

* CRITICAL: estimate the proper marginal means for `pmrm_marginals()`. `pmrm_objective()` previously reported the wrong ones for the slowing and non-proportional decline models.
* Remove unused `data` argument in `fitted()`.
* Expand `pmrm_estimates()` testing.

# pmrm 0.0.3

* Use all of `pmrm_data()` and rebuild all ordered factors in `predict()` (#5, @lbenz-lilly).
* Align `W` column mean centering between models and `predict()` (#7, @lbenz-lilly).
* Directly check for missing values in covariate columns (#9, @lbenz-lilly).

# pmrm 0.0.2

* Fix citation and links for CRAN.

# pmrm 0.0.1

* First version.
