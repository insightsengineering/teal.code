#' Get code from `Quosure`
#'
#' @name get_code
#' @param object (`Quosure`)
#' @return named `character` with the reproducible code.
#' @examples
#' q1 <- new_quosure(env = list2env(list(a = 1)), code = quote(a <- 1))
#' q2 <- eval_code(q1, code = "b <- a", name = "copy a to b")
#' q3 <- eval_code(q2, code = "# comment line")
#' q4 <- eval_code(q3, code = "d <- 2", name = "assign d")
#' get_code(q4)
#'
#' @export
setGeneric("get_code", function(object) {
  standardGeneric("get_code")
})

#' @rdname get_code
#' @export
setMethod("get_code", signature = "Quosure", function(object) {
  object@code
})
