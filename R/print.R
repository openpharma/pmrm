#' @title Print a fitted PMRM.
#' @export
#' @family visualization
#' @description Print a fitted progression model for repeated measures (PMRM).
#' @return Invisibly returns `x`, the fitted PMRM object.
#' @param x A fitted progression model for repeated measures (PMRM).
#' @param digits Non-negative integer, number of digits for rounding.
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
#'   print(fit)
print.pmrm_fit <- function(x, digits = 3L, ...) {
  metrics <- x$metrics
  for (name in names(metrics)) {
    metrics[[name]] <- round(metrics[[name]], digits = digits)
  }
  estimates <- tidy(x)
  names <- estimates$term
  estimates$term <- NULL
  estimates <- as.matrix(estimates)
  rownames(estimates) <- names
  lines <- c(
    "Model:",
    "",
    paste0(
      "  PMRM type:        ",
      if_any(x$constants$slowing, "slowing", "decline")
    ),
    paste0(
      "  Parameterization: ",
      if_any(x$constants$proportional, "proportional", "non-proportional")
    ),
    "",
    "Fit:",
    "",
    paste0(
      "  Convergence:    ",
      if_any(as.logical(x$optimization$convergence), "diverged", "converged")
    ),
    paste0("  Observations:   ", metrics$n_observations),
    paste0("  Parameters:     ", metrics$n_parameters),
    paste0("  Log likelihood: ", metrics$log_likelihood),
    paste0("  Deviance:       ", metrics$deviance),
    paste0("  AIC:            ", metrics$aic),
    paste0("  BIC:            ", metrics$bic),
    "",
    paste0("Treatment effects:"),
    "",
    paste0("  ", utils::capture.output(print(estimates)))
  )
  cat(lines, sep = "\n")
  invisible(x)
}
