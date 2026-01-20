#' @title Parameter estimates and confidence intervals
#' @export
#' @family estimates and predictions
#' @description Report parameter estimates and confidence intervals for
#'   a progression model for repeated measures (PMRM).
#' @return A `tibble` with one row for each scalar element of the
#'   selected model parameter and columns with estimates,
#'   standard errors, lower and upper bounds of two-sided
#'   normal confidence intervals, and indexing variables.
#'   If applicable, the indexing variables are `arm` and/or `visit`
#'   to indicate the study arm and study visit.
#'   If there is no obvious indexing factor in the data,
#'   then a generic integer `index` column is used.
#'   For covariance matrices, elements are identified with the
#'   `visit_row` and `visit_column` columns.
#'
#'   `beta` is not a true parameter.
#'   Instead, it is a function of `theta` and fixed at zero
#'   for the control arm and at baseline.
#'   At these marginals,
#'   the standard errors and confidence intervals for `beta` are `NA_real_`.
#' @param fit A fitted model object of class `"pmrm_fit"` returned
#'   by a `pmrm` model-fitting function.
#' @param parameter Character string, name of the type of model
#'   parameter to summarize. Must be one of `"beta"`, `"theta"`,
#'   `"alpha"`, `"gamma"`, `"sigma"`, `"rho"`, `"Sigma"`, or `"Lambda"`.
#' @param confidence Numeric between 0 and 1, the confidence level
#'   to use in 2-sided normal confidence intervals.
#' @examples
#'   set.seed(0L)
#'   simulation <- pmrm_simulate_decline_proportional(
#'     visit_times = seq_len(5L) - 1,
#'     gamma = c(1, 2)
#'   )
#'   fit <- pmrm_model_decline_proportional(
#'     data = simulation,
#'     outcome = "y",
#'     time = "t",
#'     patient = "patient",
#'     visit = "visit",
#'     arm = "arm",
#'     covariates = ~ w_1 + w_2
#'   )
#'   pmrm_estimates(fit, parameter = "beta")
#'   pmrm_estimates(fit, parameter = "alpha")
pmrm_estimates <- function(
  fit,
  parameter = c(
    "theta",
    "beta",
    "alpha",
    "gamma",
    "sigma",
    "phi",
    "rho",
    "Sigma",
    "Lambda"
  ),
  confidence = 0.95
) {
  parameter <- match.arg(parameter)
  assert(
    confidence,
    is.numeric(.),
    length(.) == 1L,
    is.finite(.),
    . >= 0,
    . <= 1,
    message = "confidence must have length 1 and be between 0 and 1."
  )
  out <- switch(
    parameter,
    theta = summarize_theta(fit),
    beta = summarize_beta(fit),
    alpha = summarize_parameter(fit, name = "alpha"),
    gamma = summarize_parameter(fit, name = "gamma"),
    sigma = summarize_sigma(fit),
    phi = summarize_phi(fit),
    rho = summarize_parameter(fit, name = "rho"),
    Lambda = summarize_covariance(fit, name = "Lambda"),
    Sigma = summarize_covariance(fit, name = "Sigma")
  )
  z <- stats::qnorm(p = (1 - confidence) / 2, lower.tail = FALSE)
  out <- dplyr::mutate(
    out,
    # Suppress warnings because betas fixed at 0 have SD = NA by convention.
    lower = suppressWarnings(estimate - z * standard_error),
    upper = suppressWarnings(estimate + z * standard_error)
  )
  dplyr::bind_cols(parameter = parameter, out)
}

summarize_theta <- function(fit) {
  UseMethod("summarize_theta")
}

#' @export
summarize_theta.pmrm_fit_decline <- function(fit) {
  arm <- fit$data[[pmrm_data_labels(fit$data)$arm]]
  tibble::tibble(
    arm = factor(levels(arm), ordered = TRUE, levels = levels(arm))[-1L],
    estimate = as.numeric(fit$estimates$theta),
    standard_error = as.numeric(fit$standard_errors$theta)
  )
}

#' @export
summarize_theta.pmrm_fit_slowing <- function(fit) {
  labels <- pmrm_data_labels(fit$data)
  arm <- fit$data[[labels$arm]]
  visit <- fit$data[[labels$visit]]
  arms <- factor(levels(arm), ordered = TRUE, levels = levels(arm))[-1L]
  visits <- factor(levels(visit), ordered = TRUE, levels = levels(visit))[-1L]
  tibble::tibble(
    arm = rep(arms, each = length(levels(visits)) - 1L),
    visit = rep(visits, times = length(levels(arms)) - 1L),
    estimate = as.numeric(t(fit$estimates$theta)),
    standard_error = as.numeric(t(fit$standard_errors$theta))
  )
}

summarize_beta <- function(fit) {
  UseMethod("summarize_beta")
}

#' @export
summarize_beta.pmrm_fit_decline <- function(fit) {
  arm <- fit$data[[pmrm_data_labels(fit$data)$arm]]
  out <- tibble::tibble(
    arm = factor(levels(arm), ordered = TRUE, levels = levels(arm)),
    estimate = as.numeric(fit$estimates$beta),
    standard_error = as.numeric(fit$standard_errors$beta)
  )
  out$standard_error[out$arm == min(out$arm)] <- NA_real_
  out
}

#' @export
summarize_beta.pmrm_fit_slowing <- function(fit) {
  labels <- pmrm_data_labels(fit$data)
  arm <- fit$data[[labels$arm]]
  visit <- fit$data[[labels$visit]]
  arms <- factor(levels(arm), ordered = TRUE, levels = levels(arm))
  visits <- factor(levels(visit), ordered = TRUE, levels = levels(visit))
  out <- tibble::tibble(
    arm = rep(arms, each = length(levels(visits))),
    visit = rep(visits, times = length(levels(arms))),
    estimate = as.numeric(t(fit$estimates$beta)),
    standard_error = as.numeric(t(fit$standard_errors$beta))
  )
  index <- out$arm == min(out$arm) | out$visit == min(out$visit)
  out$standard_error[index] <- NA_real_
  out
}

summarize_parameter <- function(fit, name) {
  tibble::tibble(
    index = seq_along(fit$estimate[[name]]),
    estimate = as.numeric(fit$estimates[[name]]),
    standard_error = as.numeric(fit$standard_errors[[name]])
  )
}

summarize_sigma <- function(fit) {
  visit <- fit$data[[pmrm_data_labels(fit$data)$visit]]
  tibble::tibble(
    visit = factor(levels(visit), ordered = TRUE, levels = levels(visit)),
    estimate = as.numeric(fit$estimates$sigma),
    standard_error = as.numeric(fit$standard_errors$sigma)
  )
}

summarize_phi <- function(fit) {
  visit <- fit$data[[pmrm_data_labels(fit$data)$visit]]
  tibble::tibble(
    visit = factor(levels(visit), ordered = TRUE, levels = levels(visit)),
    estimate = as.numeric(fit$estimates$phi),
    standard_error = as.numeric(fit$standard_errors$phi)
  )
}

summarize_covariance <- function(fit, name) {
  visit <- fit$data[[pmrm_data_labels(fit$data)$visit]]
  unique <- factor(levels(visit), ordered = TRUE, levels = levels(visit))
  visit_row <- rep(unique, each = length(unique))
  visit_column <- rep(unique, times = length(unique))
  tibble::tibble(
    visit_row = visit_row,
    visit_column = visit_column,
    estimate = as.numeric(fit$estimates[[name]]),
    standard_error = as.numeric(fit$standard_errors[[name]])
  )
}
