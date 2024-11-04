#' Code tracking with `qenv` object
#'
#' @description
#' `r badge("stable")`
#'
#' Create a `qenv` object and evaluate code in it to track code history.
#'
#' @details
#'
#' `qenv()` instantiates a `qenv` with an empty environment.
#' Any changes must be made by evaluating code in it with `eval_code` or `within`, thereby ensuring reproducibility.
#'
#' @name qenv
#'
#' @return Returns a `qenv` object.
#'
#' @seealso [`base::within()`], [`get_var()`], [`get_env()`], [`get_warnings()`], [`join()`], [`concat()`]
#' @examples
#' # create empty qenv
#' qenv()
#'
#' @export
qenv <- function() {
  q_env <- new.env(parent = parent.env(.GlobalEnv))
  lockEnvironment(q_env, bindings = TRUE)
  methods::new("qenv", env = q_env)
}
