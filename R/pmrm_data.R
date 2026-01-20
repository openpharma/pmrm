#' @title Internal function to prepare data
#' @keywords internal
#' @description Clean and annotate the data to prepare it for modeling.
#' @return A `tibble` of class `"pmrm_data"`
#'   with one row per patient visit.
#'   Rows with missing outcomes are removed,
#'   variables `arm` and `visit` are converted into ordered factors
#'   (with minimum values at the control arm and baseline, respectively),
#'   and then the rows are sorted by patient and visit.
#'   The `"labels"` attribute is a named list with the values of arguments
#'   `outcome`, `time`, `visit`, `arm`, and `covariates`.
#' @param data A `tibble` or data frame with one row per patient visit.
#'   See the arguments below for specific requirements for the data.
#' @param outcome Character string, name of the column in the data
#'   with the numeric outcome variable on the continuous scale.
#'   Could be a clinical measure of healthy or of disease severity.
#'   Baseline is part of the model, so the `outcome` should
#'   not already be a change from baseline.
#'   The vector of outcomes may have missing values, either with explicit
#'   `NA`s or with rows in the data missing for one or more visits.
#' @param time Character string, name of the column in the data
#'   with the numeric time variable on the continuous scale.
#'   This time is the time since enrollment/randomization of each patient.
#'   A time value of 0 should indicate baseline.
#' @param patient Character string, name of the column in the data
#'   with the patient ID.
#'   This vector could be a numeric, integer, factor, or character vector.
#'   `pmrm` automatically converts it into an unordered factor.
#' @param visit Character string, name of the column in the
#'   data which indicates the study visit of each row.
#'   This column could be a numeric, integer, factor, or character vector.
#'   An ordered factor is highly recommended because `pmrm`
#'   with levels assumed to be in chronological order.
#'   The minimum visit must be baseline.
#' @param arm Character string, name of the column in the
#'   data which indicates the study arm of each row.
#'   This column could be a numeric, integer, factor, or character vector.
#'   An ordered factor is highly recommended because `pmrm`
#'   automatically converts `data[[arm]]` into an ordered factor anyway.
#'   The minimum level is assumed to be the control arm.
#' @param covariates Partial right-sided formula
#'   of concomitant terms in the model
#'   for covariate adjustment (e.g. by age, gender, biomarker status, etc.).
#'   Should not include main variables such as the values of
#'   `outcome`, `time`, `patient`, `visit`, or `arm`.
#'   The columns in the data referenced in the formula must not have
#'   any missing values.
#'
#'   Set `covariates` to `~ 0` (default) to opt out of covariate adjustment.
#'   The intercept term is removed from the model matrix `W`
#'   whether or not the formula begins with `~ 0.
pmrm_data <- function(
  data,
  outcome,
  time,
  patient,
  visit,
  arm,
  covariates = ~0
) {
  assert(is.data.frame(data), message = "data must be a data frame.")
  labels <- list(
    outcome = outcome,
    time = time,
    patient = patient,
    visit = visit,
    arm = arm,
    covariates = covariates
  )
  data <- pmrm_data_new(data = data, labels = labels)
  pmrm_data_validate(data)
  pmrm_data_clean(data)
}

pmrm_data_new <- function(data, labels) {
  data <- tibble::new_tibble(data, class = "pmrm_data")
  attr(data, "pmrm_data_labels") <- labels
  data
}

pmrm_data_labels <- function(data) {
  attr(data, "pmrm_data_labels")
}

pmrm_data_validate <- function(data) {
  pmrm_predictors_validate(data)
  labels <- pmrm_data_labels(data)
  assert(
    data[[labels$outcome]],
    is.atomic(.),
    is.numeric(.),
    !all(is.na(.)),
    is.finite(.[!is.na(.)]),
    message = c(
      "the outcome column must be a numeric vector with at least some",
      "non-missing values.",
      "In addition, all non-missing values must be finite."
    )
  )
  assert(
    length(unique(data[[labels$arm]])) > 1L,
    message = "data[[arm]] must have more than one unique element."
  )
  assert(
    length(unique(data[[labels$visit]])) > 1L,
    message = "data[[visit]] must have more than one unique element."
  )
}

pmrm_predictors_validate <- function(data) {
  assert(
    data,
    tibble::is_tibble(.),
    inherits(., "pmrm_data"),
    message = "data must be a tibble of class pmrm_data."
  )
  labels <- pmrm_data_labels(data)
  outcome <- labels$outcome
  time <- labels$time
  patient <- labels$patient
  visit <- labels$visit
  arm <- labels$arm
  covariates <- labels$covariates
  envir <- environment()
  for (name in c("time", "patient", "arm", "visit")) {
    value <- get(x = name, envir = envir, inherits = FALSE)
    assert_name(
      value,
      message = paste(name, "must be a valid nonempty character string.")
    )
    assert(
      value %in% colnames(data),
      message = sprintf(
        "%s must be the name of a column in the data. Found: \"%s\"",
        name,
        outcome
      )
    )
  }
  assert(
    rlang::is_formula(covariates),
    message = "covariates argument must be a formula."
  )
  matrix <- tryCatch(
    Matrix::sparse.model.matrix(covariates, data = data),
    error = function(condition) {
      error(
        "cannot create model matrix from covariates formula: ",
        conditionMessage(condition)
      )
    }
  )
  assert(
    !anyNA(matrix),
    message = "covariate adjustment model matrix W has missing values"
  )
  assert(
    !anyDuplicated(c(outcome, time, patient, arm, visit)),
    message = paste(
      "c(outcome, time, patient, arm, visit)",
      "must not have any duplicates."
    )
  )
  for (name in c(time, patient, visit, arm)) {
    value <- data[[name]]
    assert(
      !anyNA(value),
      message = paste(name, "must not have any missing values.")
    )
  }
  for (name in c(patient, visit, arm)) {
    value <- data[[name]]
    assert(
      value,
      is.character(.) || is.numeric(.) || is.factor(.),
      message = paste(
        name,
        "must be a vector (character, numeric, or factor)."
      )
    )
    if (is.numeric(value)) {
      assert(
        is.finite(value),
        message = paste(
          "If",
          name,
          "is numeric, then all values must be finite."
        )
      )
    }
  }
  assert(
    data[[time]],
    is.atomic(.),
    is.numeric(.),
    is.finite(.),
    message = c(
      "the time column must be a numeric vector with all values",
      "finite and non-missing."
    )
  )
}

pmrm_data_clean <- function(data) {
  labels <- pmrm_data_labels(data)
  data <- data[!is.na(data[[labels$outcome]]), , drop = FALSE] # nolint
  data[[labels$patient]] <- factor(data[[labels$patient]], ordered = FALSE)
  for (name in c(labels$visit, labels$arm)) {
    value <- data[[name]]
    if (is.numeric(value)) {
      unique <- unique(value)
      levels <- as.character(unique)[order(unique)]
      data[[name]] <- ordered(as.character(value), levels = levels)
    } else if (is.character(value)) {
      levels <- sort(unique(value))
      data[[name]] <- ordered(value, levels = levels)
    } else if (is.factor(value) && !is.ordered(value)) {
      data[[name]] <- ordered(value, levels = sort(levels(value)))
    }
  }
  data[order(data[[labels$patient]], data[[labels$visit]]), , drop = FALSE] # nolint
}
