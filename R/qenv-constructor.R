#' Code tracking with `qenv` object
#'
#' @description
#' `r badge("stable")`
#'
#' Create a `qenv` object and evaluate code in it to track code history.
#'
#' @param names (`character`) for `x[names]`, names of objects included in `qenv` to subset. Names not present in `qenv`
#' are skipped. For `get_code` `r lifecycle::badge("experimental")` vector of object names to return the code for.
#' For more details see the "Extracting dataset-specific code" section.
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
