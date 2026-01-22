#' @title Plot a fitted PMRM.
#' @export
#' @family estimates
#' @description Plot a fitted progression model for repeated measures (PMRM)
#'   against the data.
#' @details The plot shows the following elements:
#'   * Raw estimates and confidence intervals on the data, as boxes
#'     (if `show_data` is `TRUE`).
#'   * Model-based estimates and confidence intervals as points and error
#'     bars, respectively
#'     (if `show_marginals` is `TRUE`).
#'   * Continuous model-based estimates and confidence bands as lines
#'     and shaded regions, respectively
#'     (if `show_predictions` is `TRUE`).
#' @return A `ggplot` object with the plot.
#' @param x A fitted model object of class `"pmrm_fit"` returned
#'   by a `pmrm` model-fitting function.
#' @param y Not used.
#' @param confidence Numeric between 0 and 1, the confidence level
#'   to use in the 2-sided confidence intervals.
#' @param show_data `TRUE` to plot data-based visit-specific data means and
#'   confidence intervals as boxes.
#'   `FALSE` to omit.
#' @param show_marginals `TRUE` to plot model-based estimates and
#'   confidence intervals as points and error bars.
#'   `FALSE` to omit.
#' @param show_marginals `TRUE` to plot model-based confidence intervals
#'   and estimates of marginal means as boxes and horizontal lines
#'   within those boxes, respectively.
#'   Uses [pmrm_marginals()] with the given level of confidence.
#'   `FALSE` to omit.
#' @param show_predictions `TRUE` to plot expected outcomes and confidence
#'   bands with lines and shaded regions, respectively.
#'   Uses [predict.pmrm_fit()] with `adjust = FALSE` and
#'   the given level of confidence
#'   on the original dataset used to fit the model.
#'   Predictions on a full dataset are generally slow,
#'   so the default is `FALSE`.
#' @param facet `TRUE` to facet the plot by study arm,
#'   `FALSE` to overlay everything in a single panel.
#' @param alpha Numeric between 0 and 1, opacity level of the
#'   model-based confidence bands.
#' @param ... Not used.
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
#'   plot(fit)
plot.pmrm_fit <- function(
  x,
  y = NULL,
  ...,
  confidence = 0.95,
  show_data = TRUE,
  show_marginals = TRUE,
  show_predictions = FALSE,
  facet = TRUE,
  alpha = 0.25
) {
  assert(
    confidence,
    is.numeric(.),
    length(.) == 1L,
    is.finite(.),
    . >= 0,
    . <= 1,
    message = "confidence must have length 1 and be between 0 and 1."
  )
  assert(
    show_data,
    isTRUE(.) || isFALSE(.),
    message = "show_data must be TRUE or FALSE."
  )
  assert(
    facet,
    isTRUE(.) || isFALSE(.),
    message = "facet must be TRUE or FALSE."
  )
  assert(
    alpha,
    is.numeric(.),
    length(.) == 1L,
    is.finite(.),
    . >= 0,
    . <= 1,
    message = "alpha must have length 1 and be between 0 and 1."
  )
  z <- stats::qnorm(p = (1 - confidence) / 2, lower.tail = FALSE)
  out <- ggplot2::ggplot()
  labels <- pmrm_data_labels(x$data)
  if (show_data) {
    raw <- tibble::tibble(
      outcome = x$data[[labels$outcome]],
      time = x$data[[labels$time]],
      visit = x$data[[labels$visit]],
      arm = as.character(x$data[[labels$arm]])
    )
    grouped <- dplyr::group_by(raw, visit, arm)
    summaries <- dplyr::summarize(
      grouped,
      estimate = mean(outcome, na.rm = TRUE),
      standard_error = sd(outcome, na.rm = TRUE) / sqrt(dplyr::n()),
      lower = estimate - z * standard_error,
      upper = estimate + z * standard_error,
      time = mean(time),
      .groups = "drop"
    )
    out <- out +
      ggplot2::geom_crossbar(
        data = summaries,
        mapping = ggplot2::aes(
          x = time,
          y = estimate,
          ymin = lower,
          ymax = upper,
          color = arm
        ),
        width = min(diff(x$constants$spline_knots)) / 2
      )
  }
  if (show_marginals) {
    marginals <- dplyr::mutate(
      pmrm_marginals(x, type = "outcome", confidence = confidence),
      arm = as.character(arm)
    )
    out <- out +
      ggplot2::geom_point(
        data = marginals,
        mapping = ggplot2::aes(x = time, y = estimate, color = arm)
      ) +
      ggplot2::geom_errorbar(
        data = marginals,
        mapping = ggplot2::aes(
          x = time,
          ymin = lower,
          ymax = upper,
          color = arm
        ),
        width = min(diff(x$constants$spline_knots)) / 4
      )
  }
  if (show_predictions) {
    predictions <- dplyr::mutate(
      predict(
        object = x,
        data = x$data,
        adjust = FALSE,
        confidence = confidence
      ),
      arm = as.character(arm)
    )
    out <- out +
      ggplot2::geom_line(
        data = predictions,
        mapping = ggplot2::aes(x = time, y = estimate, color = arm)
      ) +
      ggplot2::geom_ribbon(
        data = predictions,
        mapping = ggplot2::aes(
          x = time,
          ymin = lower,
          ymax = upper,
          fill = arm
        ),
        alpha = alpha
      )
  }
  if (facet) {
    out <- out +
      facet_wrap(~arm)
  }
  out
}
