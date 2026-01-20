assert <- function(
  value = NULL,
  ...,
  message = NULL,
  envir = parent.frame()
) {
  expr <- match.call(expand.dots = FALSE)$...
  if (!length(expr)) {
    expr <- list(quote(.))
  }
  conditions <- lapply(
    expr,
    function(expr) all(eval(expr, envir = list(. = value), enclos = envir))
  )
  if (!all(unlist(conditions))) {
    chr_expr <- lapply(expr, function(x) sprintf("all(%s)", deparse(x)))
    chr_expr <- paste(unlist(chr_expr), collapse = " && ")
    chr_value <- deparse(substitute(value))
    out <- sprintf("%s is not true on . = %s", chr_expr, chr_value)
    error(if_any(is.null(message), out, message))
  }
}

assert_name <- function(value, message = NULL) {
  assert(value, is.character(.), !anyNA(.), nzchar(.), message = message)
  assert_scalar(value = value, message = message)
}

assert_numeric <- function(value, message = NULL) {
  assert(value, is.numeric(.), !anyNA(.), message = message)
}

assert_nonnegative <- function(value, message = NULL) {
  assert_numeric(value, message = message)
  assert(value >= 0, message = message)
}

assert_positive <- function(value, message = NULL) {
  assert_numeric(value, message = message)
  assert(value > 0, message = message)
}

assert_scalar <- function(value, message = NULL) {
  assert(length(value) == 1L, message = message)
}
