#' The Names of a `qenv` or `qenv_error` Object
#'
#' Functions to get the names of a `qenv` or `qenv_error` object.
#' The names are extrapolated from the objects in the `qenv` environment and
#' are not stored statically, unlike the normal behavior of `names()` function.
#'
#' Objects named with a `.` (dot) prefix will be ignored and not returned.
#'
#' @param x (`qenv` or `qenv_error`) object.
#' @return A character vector of names.
#'
#' @seealso [base::names()]
#'
#' @examples
#' q1 <- within(qenv(), iris <- iris)
#' names(q1)
#'
#' q2 <- within(q1, {
#'   mtcars <- mtcars
#'   CO2 <- CO2
#' })
#' names(q2)
#'
#' @export
names.qenv <- function(x) {
  ls(get_env(x))
}

#' @rdname names.qenv
#' @export
names.qenv.error <- function(x) {
  NULL
}
