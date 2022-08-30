#' Get object from the `Quosure` environment
#'
#' Get object from the `Quosure` environment.
#' @param object (`Quosure`)
#' @param var (`character(1)`) name of the variable to pull from the environment.
#' @name get_var
#' @export
setGeneric("get_var", function(object, var) {
  standardGeneric("get_var")
})


#' @rdname get_var
#' @export
setMethod("get_var", signature = c("Quosure", "character"), function(object, var) {
  get(var, envir = object@env)
})

#' @param x (`Quosure`)
#' @param i (`character`) name of the binding in environment (name of the variable)
#' @param j not used
#' @param ... not used
#' @rdname get_var
#' @export
setMethod("[[", c("Quosure", "ANY", "missing"), function(x, i, j, ...) {
  get_var(x, i)
})
