#' Evaluate the code in the `Quosure` environment
#'
#' Given code is evaluated in the `Quosure` environment and appended to the `code` slot. This means
#' that state of the environment is always a result of the stored code (if `Quosure` was initialized)
#' with reproducible code.
#'
#' @name eval_code
#'
#' @param object (`Quosure`)
#' @param code (`character` or `language`) code to evaluate. Also accepts and stores comments
#' @param name (`character(1)`) name of the code block.
#'
#' @examples
#' q1 <- new_quosure(env = list2env(list(a = 1)), code = quote(a <- 1))
#' q2 <- eval_code(q1, "library(checkmate)")
#' q3 <- eval_code(q2, "assert_number(a)")
#'
#' @export
setGeneric("eval_code", function(object, code, name = "code") {
  if (inherits(try(object, silent = TRUE), "try-error")) {
    return(object)
  }
  standardGeneric("eval_code")
})

#' @rdname eval_code
#' @export
setMethod(
  "eval_code",
  signature = c("Quosure", "character"),
  function(object, code, name) {
    checkmate::assert_string(name)
    if (is.null(names(code))) {
      code <- paste(code, collapse = "\n")
      names(code) <- name
    }
    id <- sample.int(.Machine$integer.max, size = length(code))

    evaluated_code <- object@code

    object@id <- c(object@id, id)
    object@code <- .keep_code_name_unique(object@code, code)

    # need to copy the objects from old env to new env
    # to avoid updating environments in the separate objects
    object@env <- .copy_env(object@env)
    eval(parse(text = code), envir = object@env)
    lockEnvironment(object@env)
    object

  }
)

#' @rdname eval_code
#' @export
setMethod(
  "eval_code",
  signature = c("Quosure", "expression"),
  function(object, code, name) {
    code_char <- as.character(code)
    eval_code(object, code_char, name = name)
  }
)

#' @rdname eval_code
#' @export
setMethod(
  "eval_code",
  signature = c("Quosure", "language"),
  function(object, code, name) {
    code_char <- as.expression(code)
    eval_code(object, code_char, name = name)
  }
)

#' @rdname eval_code
#' @export
setMethod(
  "eval_code",
  signature = c("error", "ANY"),
  function(object, code, name) {
    object
  }
)
