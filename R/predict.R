#' @title Predict new outcomes
#' @export
#' @family estimates and predictions
#' @description Return the expected values, standard errors,
#'   and confidence intervals of new outcomes.
#' @return A `tibble` with one row for each row in the `data` argument and
#'   columns `"estimate"`, `"standard_error"`, `"lower"`, and `"upper"`.
#'   Columns `"lower"` and `"upper"` are lower and upper bounds of 2-sided
#'   confidence intervals on the means.
#'   (The confidence intervals are not actually truly prediction intervals
#'   because they do not include variability from residuals.)
#' @param object A fitted model object of class `"pmrm_fit"`
#'   produced by [pmrm_model_decline()] or [pmrm_model_slowing()].
#' @param data A `tibble` or data frame with one row per patient visit.
#'   This is the new data for making predictions.
#'   It must have all the same columns as the original you fit with the model,
#'   except that the outcome column can be entirely absent.
#'   `object$data` is an example dataset that will work.
#'   It is just like the original data, except that rows with missing
#'   responses are removed, and the remaining rows are sorted
#'   by patient ID and categorical scheduled visit.
#' @param adjust `TRUE` or `FALSE`.
#'    `adjust = TRUE` returns estimates and inference for
#'    covariate-adjusted `mu_ij`
#'    values (defined in `vignette("models", package = "pmrm")`) for new data.
#'    `adjust = FALSE` instead returns inference on `mu_ij - W %*% gamma`,
#'    the non-covariate-adjusted predictions useful in plotting a
#'    continuous disease progression trajectory in [plot.pmrm_fit()].
#' @param confidence Numeric between 0 and 1, the confidence level
#'   to use in the 2-sided confidence intervals.
#' @param ... Not used.
#' @examples
#'   set.seed(0L)
#'   simulation <- pmrm_simulate_decline(
#'     visit_times = seq_len(5L) - 1,
#'     gamma = c(1, 2)
#'   )
#'   fit <- pmrm_model_decline(
#'     data = simulation,
#'     outcome = "y",
#'     time = "t",
#'     patient = "patient",
#'     visit = "visit",
#'     arm = "arm",
#'     covariates = ~ w_1 + w_2
#'   )
#'   new_data <- pmrm_simulate_decline(
#'     patients = 1,
#'     visit_times = seq_len(5L) - 1,
#'     gamma = c(1, 2)
#'   )
#'   new_data$y <- NULL # Permitted but not strictly necessary.
#'   predict(fit, new_data)
predict.pmrm_fit <- function(
  object,
  data = object$data,
  adjust = TRUE,
  confidence = 0.95,
  ...
) {
  assert(
    adjust,
    isTRUE(.) || isFALSE(.),
    message = "adjust must be TRUE or FALSE."
  )
  assert(
    confidence,
    is.numeric(.),
    length(.) == 1L,
    is.finite(.),
    . >= 0,
    . <= 1,
    message = "confidence must have length 1 and be between 0 and 1."
  )
  labels <- pmrm_data_labels(object$data)
  data_new <- pmrm_data_new(data = data, labels = labels)
  pmrm_predictors_validate(data_new)
  data_new[[labels$outcome]] <- NA_real_
  data_old <- object$data
  patient <- labels$patient
  data_new[[patient]] <- paste0("new_", data_new[[patient]])
  data_old[[patient]] <- paste0("old_", data_old[[patient]])
  data_combined <- dplyr::bind_rows(data_old, data_new)
  data_combined[[patient]] <- factor(
    data_combined[[patient]],
    levels = unique(data_combined[[patient]])
  )
  constants <- pmrm_constants(
    data = data_combined,
    visit_times = object$constants$visit_times,
    spline_knots = object$constants$spline_knots,
    spline_method = object$constants$spline_method,
    reml = FALSE,
    hessian = object$constants$hessian,
    slowing = object$constants$slowing,
    proportional = object$constants$proportional,
    predict = TRUE,
    adjust = adjust
  )
  parameters <- object$optimization$par
  model <- RTMB::MakeADFun(
    func = function(parameters) object$objective(constants, parameters),
    parameters = object$initial,
    random = object$constants$random,
    silent = object$options$silent
  )
  hessian <- stats::optimHess(par = parameters, fn = model$fn, gr = model$gr)
  report <- RTMB::sdreport(
    obj = model,
    par.fixed = parameters,
    hessian.fixed = hessian,
    getReportCovariance = FALSE
  )
  summary <- summary(report)
  name <- ifelse(adjust, "mu", "mu_unadjusted")
  summary <- tibble::as_tibble(summary[rownames(summary) == name, ])
  summary <- summary[-seq_len(nrow(data_old)), , drop = FALSE] # nolint
  z <- stats::qnorm(p = (1 - confidence) / 2, lower.tail = FALSE)
  tibble::tibble(
    arm = data[[labels$arm]],
    visit = data[[labels$visit]],
    time = data[[labels$time]],
    estimate = summary[["Estimate"]],
    standard_error = summary[["Std. Error"]],
    lower = estimate - z * standard_error,
    upper = estimate + z * standard_error
  )
}

#' @export
stats::predict
