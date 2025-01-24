#' Get object from `qenv`
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Instead of [get_var()] use native \R operators/functions:
#' `x[[name]]`, `x$name` or [get()]:
#'
#' Retrieve variables from the `qenv` environment.
#'
#' @param object,x (`qenv`)
#' @param var,i (`character(1)`) variable name.
#'
#' @return The value of required variable (`var`) within `qenv` object.
#'
#' @examples
#' q <- qenv()
#' q1 <- eval_code(q, code = quote(a <- 1))
#' q2 <- eval_code(q1, code = "b <- a")
#' get_var(q2, "b")
#'
#' @aliases get_var,qenv,character-method
#' @aliases get_var,qenv.error,ANY-method
#'
#' @export
setGeneric("get_var", function(object, var) {
  dev_suppress(object)
  standardGeneric("get_var")
})

setMethod("get_var", signature = c("qenv", "character"), function(object, var) {
  lifecycle::deprecate_soft("0.6.0", "get_var()", "base::get()")
  tryCatch(
    get(var, envir = object@.xData, inherits = FALSE),
    error = function(e) {
      message(conditionMessage(e))
      NULL
    }
  )
})

setMethod("get_var", signature = c("qenv.error", "ANY"), function(object, var) {
  stop(errorCondition(
    list(message = conditionMessage(object)),
    class = c("validation", "try-error", "simpleError")
  ))
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
