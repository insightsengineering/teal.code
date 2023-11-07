#' Replace code
#'
#' Replace last element of code stored in `qenv`.
#'
#' Replace the last element of the character vector stored in `@code` with provided `code`.
#' This can be used to mask sensitive information, e.g. credentials.
#'
#' @param object `qenv`
#' @param code (`character` or `language`) code to set
#'
#' @return A modified `qenv`.
#'
#' @seealso [`qenv-class`], [`eval_code`]
#'
#' @examples
#' q <- new_qenv()
#' q <- eval_code(q, expression(i <- iris, ii <- subset(i, Species == "virginica")))
#' get_code(q)
#' q <- replace_code(q, expression(i <- iris, ii <- subset(i, Species != "virginica")))
#' get_code(q)

#' @name replace_code
#' @rdname replace_code
#' @aliases replace_code,qenv,character-method
#' @aliases replace_code,qenv,language-method
#' @aliases replace_code,qenv,expression-method
#' @aliases replace_code,qenv.error,ANY-method

#' @export
setGeneric("replace_code", function(object, code) standardGeneric("replace_code"))

#' @export
setMethod("replace_code", signature = c("qenv", "character"), function(object, code) {
  masked_code <- get_code(object)
  masked_code[length(masked_code)] <- code
  object@code <- masked_code
  object
})

#' @export
setMethod("replace_code", signature = c("qenv", "language"), function(object, code) {
  replace_code(object, code = format_expression(code))
})

#' @export
setMethod("replace_code", signature = c("qenv", "expression"), function(object, code) {
  replace_code(object, code = format_expression(code))
})

#' @export
setMethod("replace_code", signature = c("qenv.error", "ANY"), function(object, code) {
  object
})
