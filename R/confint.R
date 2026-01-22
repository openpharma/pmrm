#' @title Confidence intervals of parameters
#' @export
#' @family model comparison
#' @description Compute confidence intervals of the family of model
#'   parameters specified in `parm`.
#' @details See `vignette("models", package = "pmrm")` for details.
#' @return A numeric matrix with one row for each treatment effect
#'   parameter (`theta`) and named columns with the lower and upper
#'   bounds of 2-sided confidence intervals on the parameters.
#' @inheritParams stats::confint
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
#'   confint(fit)
confint.pmrm_fit <- function(object, parm = NULL, level = 0.95, ...) {
  estimates <- pmrm_estimates(
    fit = object,
    parameter = "theta",
    confidence = level
  )
  out <- as.matrix(estimates[, c("lower", "upper"), drop = FALSE])
  bounds <- c((1 - level) / 2, 1 - (1 - level) / 2)
  colnames(out) <- paste(
    format(100 * bounds, trim = TRUE, scientific = FALSE, digits = 3),
    "%"
  )
  if (object$constants$proportional) {
    rownames(out) <- estimates$arm
  } else {
    rownames(out) <- paste(estimates$arm, estimates$visit, sep = ":")
  }
  if (!is.null(parm)) {
    assert(
      parm %in% rownames(out),
      message = paste(
        "In confint.pmrm_fit(), all elements of parm must be in",
        "names(coef()) on the fitted model object."
      )
    )
    out <- out[parm, , drop = FALSE]
  }
  out
}

#' @export
stats::confint
