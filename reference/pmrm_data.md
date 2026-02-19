# Internal function to prepare data

Clean and annotate the data to prepare it for modeling.

## Usage

``` r
pmrm_data(
  data,
  outcome,
  time,
  patient,
  visit,
  arm,
  covariates = ~0,
  subset = FALSE
)
```

## Arguments

- data:

  A `tibble` or data frame with one row per patient visit. See the
  arguments below for specific requirements for the data.

- outcome:

  Character string, name of the column in the data with the numeric
  outcome variable on the continuous scale. Could be a clinical measure
  of healthy or of disease severity. Baseline is part of the model, so
  the `outcome` should not already be a change from baseline. The vector
  of outcomes may have missing values, either with explicit `NA`s or
  with rows in the data missing for one or more visits.

- time:

  Character string, name of the column in the data with the numeric time
  variable on the continuous scale. This time is the time since
  enrollment/randomization of each patient. A time value of 0 should
  indicate baseline.

- patient:

  Character string, name of the column in the data with the patient ID.
  This vector could be a numeric, integer, factor, or character vector.
  `pmrm` automatically converts it into an unordered factor.

- visit:

  Character string, name of the column in the data which indicates the
  study visit of each row. This column could be a numeric, integer,
  factor, or character vector. An ordered factor is highly recommended
  because `pmrm` with levels assumed to be in chronological order. The
  minimum visit must be baseline.

- arm:

  Character string, name of the column in the data which indicates the
  study arm of each row. This column could be a numeric, integer,
  factor, or character vector. An ordered factor is highly recommended
  because `pmrm` automatically converts `data[[arm]]` into an ordered
  factor anyway. The minimum level is assumed to be the control arm.

- covariates:

  Partial right-sided formula of concomitant terms in the model for
  covariate adjustment (e.g. by age, gender, biomarker status, etc.).
  Should not include main variables such as the values of `outcome`,
  `time`, `patient`, `visit`, or `arm`. The columns in the data
  referenced in the formula must not have any missing values.

  Set `covariates` to `~ 0` (default) to opt out of covariate
  adjustment. The intercept term is removed from the model matrix `W`
  whether or not the formula begins with \`~ 0.

- subset:

  `TRUE` if `data` is a subset of data without all levels of `visit` or
  `arm`, `FALSE` to expect a full dataset when validating.

## Value

A `tibble` of class `"pmrm_data"` with one row per patient visit. Rows
with missing outcomes are removed, variables `arm` and `visit` are
converted into ordered factors (with minimum values at the control arm
and baseline, respectively), and then the rows are sorted by patient and
visit. The `"labels"` attribute is a named list with the values of
arguments `outcome`, `time`, `visit`, `arm`, and `covariates`.
