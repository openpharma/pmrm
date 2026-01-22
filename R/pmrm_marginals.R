#' @title Marginal means
#' @export
#' @family estimates
#' @description Report the estimates and standard errors of marginal means
#'   at each study arm and visit. The assumed visit times should have been
#'   given in the `marginals` argument of the model-fitting function.
#'   Use the `type` argument to choose
#'   marginal means of the outcomes, marginal estimates of change from
#'   baseline, and marginal estimates of treatment effects.
#' @return A `tibble` with one row per marginal mean and columns with the
#'   estimate, standard error, 2-sided confidence bounds, and indicator
#'   columns.
#' @param fit A `pmrm` fitted model object returned by a model-fitting
#'   function.
#' @param type Character string.
#'   `"outcome"` reports marginal means on the outcome scale,
#'   `"change"` reports estimates of change from baseline,
#'   and `"effect"` reports estimates of treatment effects
#'   (change from baseline of each active arm minus that of the control arm.)
#' @param confidence A numeric from 0 to 1 with the confidence level
#'   for confidence intervals.
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
#'   pmrm_marginals(fit)
pmrm_marginals <- function(
  fit,
  type = c("outcome", "change", "effect"),
  confidence = 0.95
) {
  type <- match.arg(type)
  assert(
    inherits(fit, "pmrm_fit"),
    message = "fit must be a pmrm fitted model object."
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
  reported <- summary(fit$report)
  name <- paste0("marginal_", type)
  marginals <- reported[rownames(reported) == name, , drop = FALSE] # nolint
  marginals <- tibble::as_tibble(marginals)
  colnames(marginals) <- c("estimate", "standard_error")
  labels <- pmrm_data_labels(fit$data)
  arm <- fit$data[[labels$arm]]
  visit <- fit$data[[labels$visit]]
  K <- fit$constants$K
  J <- fit$constants$J
  z <- stats::qnorm(p = (1 - confidence) / 2, lower.tail = FALSE)
  marginals <- mutate(
    marginals,
    lower = estimate - z * standard_error,
    upper = estimate + z * standard_error,
    arm = rep(sort(unique(arm)), each = J),
    visit = rep(sort(unique(visit)), times = K),
    time = fit$constants$marginal_t
  )
  columns <- c(
    "arm",
    "visit",
    "time",
    "estimate",
    "standard_error",
    "lower",
    "upper"
  )
  marginals[, columns]
}
