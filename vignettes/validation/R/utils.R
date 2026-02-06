get_package_data <- function(name, package) {
  envir <- new.env(parent = emptyenv())
  data(list = name, package = package, envir = envir)
  envir[[name]]
}
