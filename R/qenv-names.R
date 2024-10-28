#' The Names of a `qenv` or `qenv_error` Object
#'
#' Functions to get the names of a `qenv` or `qenv_error` object.
#' The names are extrapolated from the objects in the `qenv` environment and
#' are not stored statically, unlike the normal behavior of `names()` function.
#'
#' Objects named with a `.` (dot) prefix will be ignored and not returned,
#' unless `all.names` parameter is set to `TRUE`.
#'
#' @param x A (`qenv` or `qenv_error`) object.
#' @param all.names (`logical(1)`) that specifies whether to include hidden
#' objects.
#' @param value Does nothing as the names assignment is not supported.
#'
#' @return A character vector of names.
#'
#' @seealso [base::names()]
#'
#' @export
names.qenv <- function(x, all.names = FALSE) {
  checkmate::assert_flag(all.names)
  ls(get_env(x), all.names = all.names)
}

#' @rdname names.qenv
#' @export
names.qenv.error <- function(x, all.names = FALSE) {
  NULL
}

#' @rdname names.qenv
#' @export
`names<-.qenv` <- function(x, value) {
  warning("`names(x) <- value` assignment does nothing for qenv objects")
  x
}

#' @rdname names.qenv
#' @export
`names<-.qenv.error` <- function(x, value) {
  warning("`names(x) <- value` assignment does nothing for qenv.error objects")
  x
}
