error <- function(...) {
  rlang::abort(
    message = paste0(...),
    class = c("pmrm_error", "pmrm"),
    .frame = emptyenv()
  )
}

warn <- function(...) {
  rlang::warn(
    message = paste0(...),
    class = c("pmrm_warning", "pmrm")
  )
}

deprecate <- function(
  name,
  date,
  version,
  alternative,
  condition = "warning",
  value = "x",
  frequency = "always"
) {
  if (is.null(value)) {
    return(invisible())
  }
  message <- sprintf(
    "%s was deprecated on %s (pmrm version %s). Alternative: %s.",
    name,
    date,
    version,
    alternative
  )
  class <- c("pmrm_warning", "pmrm")
  if_any(
    condition == "warning",
    rlang::warn(
      message = message,
      class = c("pmrm_deprecate", class),
      .frequency = frequency,
      .frequency_id = name
    ),
    rlang::inform(
      message = message,
      class = c("pmrm_deprecate", class),
      .frequency = frequency,
      .frequency_id = name
    )
  )
  invisible()
}
