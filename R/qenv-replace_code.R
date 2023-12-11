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
#' @section Examples:
#' `q <- qenv()` \cr
#' `q <- eval_code(q, expression(i <- iris, ii <- subset(i, Species == "virginica")))` \cr
#' `get_code(q)` \cr
#' `q <- replace_code(q, expression(i <- iris, ii <- subset(i, Species != "virginica")))` \cr
#' `get_code(q)` \cr

#' @name replace_code
#' @rdname replace_code
#' @aliases replace_code,qenv,character-method
#' @aliases replace_code,qenv,language-method
#' @aliases replace_code,qenv,expression-method
#' @aliases replace_code,qenv.error,ANY-method

#' @keywords internal
setGeneric("replace_code", function(object, code) standardGeneric("replace_code"))

#' @keywords internal
setMethod("replace_code", signature = c("qenv", "character"), function(object, code) {
  masked_code <- get_code(object)
  code_lines <- unlist(strsplit(masked_code, "\n"))

  if (!is.null(code_lines)) {
    code_lines[length(code_lines)] <- code
    object@code <- paste(code_lines, collapse = "\n")
  }

  object
})

#' @keywords internal
setMethod("replace_code", signature = c("qenv", "language"), function(object, code) {
  replace_code(object, code = paste(lang2calls(code), collapse = "\n"))
})

#' @keywords internal
setMethod("replace_code", signature = c("qenv", "expression"), function(object, code) {
  replace_code(object, code = paste(lang2calls(code), collapse = "\n"))
})

#' @keywords internal
setMethod("replace_code", signature = c("qenv.error", "ANY"), function(object, code) {
  object
})
