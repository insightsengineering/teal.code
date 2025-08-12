#' Get object from `qenv`
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Instead of [get_var()] use native \R operators/functions:
#' `x[[name]]`, `x$name` or [get()]:
#' 
#' @param ... function is deprecated.
#'
#' @aliases get_var,qenv-method
#' @aliases get_var,qenv.error-method
#'
#' @export
setGeneric("get_var", function(...) {
  standardGeneric("get_var")
})

setMethod("get_var", signature = c("qenv"), function(...) {
  lifecycle::deprecate_stop("0.6.0", "get_var()", "base::get()")
})

setMethod("get_var", signature = c("qenv.error"), function(...) {
  lifecycle::deprecate_stop("0.6.0", "get_var()", "base::get()")
})

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
