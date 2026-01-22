#' @title Tidy a fitted PMRM.
#' @export
#' @family estimates
#' @description Return tidy parameter summaries of a progression model
#'   for repeated measures (PMRM).
#' @return A tidy `tibble` with one row for each treatment effect
#'   model parameter (`theta`) and columns with the parameter name
#'   (study arm and/or visit it corresponds to), estimate,
#'   and standard error. This format aligns with the `tidy()`
#'   method of similar fitted models in R.
#' @param x A fitted progression model for repeated measures (PMRM).
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
#'   tidy(fit)
tidy.pmrm_fit <- function(x, ...) {
  out <- pmrm_estimates(fit = x, parameter = "theta")
  if (x$constants$proportional) {
    out$term <- as.character(out$arm)
  } else {
    out$term <- paste(out$arm, out$visit, sep = ":")
  }
  out$std.error <- out$standard_error
  out[, c("term", "estimate", "std.error")]
}

#' @export
generics::tidy
