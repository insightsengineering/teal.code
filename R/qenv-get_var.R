#' Get object from the `qenv` environment
#'
#' Get object from the `qenv` environment.
#' @param object (`qenv`)
#' @param var (`character(1)`) name of the variable to pull from the environment.
#' @name get_var
#' @examples
#' q1 <- new_qenv(env = list2env(list(a = 1)), code = quote(a <- 1))
#' q2 <- eval_code(q1, code = "b <- a")
#' get_var(q2, "b")
#' q2[["b"]]
#'
#' @export
setGeneric("get_var", function(object, var) {
  grDevices::pdf(nullfile())
  on.exit(grDevices::dev.off())
  standardGeneric("get_var")
})


#' @rdname get_var
#' @export
setMethod("get_var", signature = c("qenv", "character"), function(object, var) {
  tryCatch(
    get(var, envir = object@env),
    error = function(e) {
      message(conditionMessage(e))
      NULL
    }
  )
})

#' @rdname get_var
#' @export
setMethod("get_var", signature = "qenv.error", function(object, var) {
  stop(errorCondition(
    list(message = conditionMessage(object)),
    class = c("validation", "try-error", "simpleError")
  ))
})


#' @param x (`qenv`)
#' @param i (`character`) name of the binding in environment (name of the variable)
#' @param j not used
#' @param ... not used
#' @rdname get_var
#' @export
setMethod("[[", signature = c("qenv", "ANY", "missing"), function(x, i, j, ...) {
  get_var(x, i)
})

#' @rdname get_var
#' @export
`[[.qenv.error` <- function(x, i, j, ...) {
  stop(errorCondition(
    list(message = conditionMessage(x)),
    class = c("validation", "try-error", "simpleError")
  ))
}
