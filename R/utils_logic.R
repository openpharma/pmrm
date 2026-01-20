if_any <- function(condition, true, false) {
  if (any(condition)) {
    true
  } else {
    false
  }
}
