#' Get object from the `Quosure` environment
#'
#' Get object from the `Quosure` environment.
#' @param object (`Quosure`)
#' @param var (`character(1)`) name of the variable to pull from the environment.
#' @name get_var
#' @examples
#' q1 <- new_quosure(env = list2env(list(a = 1)), code = quote(a <- 1))
#' q2 <- eval_code(q1, code = "b <- a")
#' get_var(q2, "b")
#' q2[["b"]]
#'
#' @export
setGeneric("get_var", function(object, var) {
  standardGeneric("get_var")
})


#' @rdname get_var
#' @export
setMethod("get_var", signature = c("Quosure", "character"), function(object, var) {
  get(var, envir = object@env)
})

#' @rdname get_var
#' @export
setMethod("get_var", signature = "errors", function(object, var) {
  object
})


#' @param x (`Quosure`)
#' @param i (`character`) name of the binding in environment (name of the variable)
#' @param j not used
#' @param ... not used
#' @rdname get_var
#' @export
setMethod("[[", signature = c("Quosure", "ANY", "missing"), function(x, i, j, ...) {
  get_var(x, i)
})

#' @rdname get_var
#' @export
setMethod("[[", signature = "errors", function(x, i, j, ...) {
  print("\n\n\nelo\n\n\n")
  x
})
