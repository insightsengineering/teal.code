#' Get object from `qenv`
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
#' q2[["b"]]
#'
#' @name get_var
#' @rdname get_var
#' @aliases get_var,qenv,character-method
#' @aliases get_var,qenv.error,ANY-method
#'
#' @export
setGeneric("get_var", function(object, var) {
  grDevices::pdf(nullfile())
  on.exit(grDevices::dev.off())
  standardGeneric("get_var")
})

setMethod("get_var", signature = c("qenv", "character"), function(object, var) {
  tryCatch(
    get(var, envir = object@env, inherits = FALSE),
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
setMethod("[[", signature = c("qenv", "ANY"), function(x, i) {
  get_var(x, i)
})

#' @export
`[[.qenv.error` <- function(x, i) {
  stop(errorCondition(
    list(message = conditionMessage(x)),
    class = c("validation", "try-error", "simpleError")
  ))
}
