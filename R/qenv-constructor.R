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
#' `qenv()` instantiates a with an empty `qenv` environment.
#'
#' @section `qenv` characteristics:
#'
#' A `qenv` inherits from the `environment` class, behaves like an environment, and has the
#' following characteristics:
#'
#' -	The environment is locked, and data modification is only possible through the `eval_code()`
#' and `within()` functions.
#' - It stores metadata about the code used to create the data.
#' - Is immutable which means that each code evaluation does not modify the original `qenv` object
#' directly.
#'
#' @name qenv
#'
#' @return `qenv` returns a `qenv` object.
#'
#' @seealso [`base::within()`], [`get_var()`], [`get_env()`], [`get_warnings()`], [`join()`], [`concat()`]
#' @examples
#' # create empty qenv
#' qenv()
#'
#' @export
qenv <- function() {
  methods::new("qenv")
}
