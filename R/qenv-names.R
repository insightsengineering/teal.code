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
#' @param ... Additional parameters to this function, allowed parameters:
#'
#' - `all.names`: (`logical(1)`)that specifies whether to include hidden objects.
#' @param value Does nothing as the names assignment is not supported.
#'
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
names.qenv <- function(x, ...) {
  dots <- rlang::list2(...)
  if (length(setdiff(names(dots), "all.names")) > 0) {
    stop("Only `x` and 'all.names' parameter are allowed")
  }
  checkmate::assert_flag(dots[["all.names"]], .var.name = "all.names", null.ok = TRUE)
  if (is.null(dots[["all.names"]])) dots[["all.names"]] <- FALSE
  ls(get_env(x), all.names = dots[["all.names"]])
}

#' @rdname names.qenv
#' @export
names.qenv.error <- function(x) {
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
