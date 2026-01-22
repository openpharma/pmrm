#' @importFrom dplyr bind_rows group_by mutate n select summarize
#' @importFrom generics glance tidy
#' @importFrom ggplot2 aes facet_wrap geom_crossbar geom_errorbar
#'   geom_point ggplot position_dodge
#' @importFrom Matrix Matrix
#' @importFrom nlme VarCorr
#' @importFrom Matrix colMeans sparse.model.matrix
#' @importFrom rlang abort inform is_formula warn
#' @importFrom stats AIC BIC as.formula confint deviance fitted lm logLik
#'   median nlminb rnorm optimHess predict residuals sd update
#' @importFrom RTMB dgmrf MakeADFun matrix
#' @importFrom tibble as_tibble is_tibble new_tibble
#' @importFrom tidyselect everything
#' @importFrom vctrs vec_cbind
NULL

utils::globalVariables(
  names = c(
    ".",
    "adjust",
    "alpha",
    "arm",
    "estimate",
    "gamma",
    "i",
    "index_beta_fitted",
    "I",
    "j",
    "J",
    "k",
    "kj",
    "K",
    "lower",
    "marginal_j",
    "marginal_k",
    "marginal_kj",
    "marginal_t",
    "n_visits",
    "parameter",
    "phi",
    "proportional",
    "outcome",
    "reml",
    "rho",
    "Sigma",
    "sigma",
    "slowing",
    "spline_knots",
    "spline_method",
    "standard_error",
    "theta",
    "time",
    "upper",
    "visit",
    "W",
    "W_column_means"
  ),
  package = "pmrm"
)
