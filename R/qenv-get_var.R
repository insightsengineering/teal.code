#' Get object from `qenv`
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Instead of [get_var()] use native \R operators/functions:
#' `x[[name]]`, `x$name` or [get()]:
#'
#' @param ... function is deprecated.
#' @param x (`qenv`)
#' @param i (`character(1)`) variable name.
#'
#' @export
get_var <- function(...) lifecycle::deprecate_stop("0.7.0", "get_var()", "base::get()")

#' @rdname get_var
#' @export
`[[.qenv.error` <- function(x, i) {
  stop(errorCondition(
    list(message = conditionMessage(x)),
    class = c("validation", "try-error", "simpleError")
  ))
}

#' @export
names.qenv.error <- function(x) NULL

#' @export
`$.qenv.error` <- function(x, name) {
  # Must allow access of elements in qenv.error object (message, call, trace, ...)
  # Otherwise, it will enter an infinite recursion with the `conditionMessage(x)` call.
  if (exists(name, x)) {
    return(NextMethod("$", x))
  }

  class(x) <- setdiff(class(x), "qenv.error")
  stop(errorCondition(
    list(message = conditionMessage(x)),
    class = c("validation", "try-error", "simpleError")
  ))
}
