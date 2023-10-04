#' Get code from `qenv`
#'
#' @name get_code
#' @param object (`qenv`)
#' @param deparse (`logical(1)`) if the returned code should be converted to character
#' @param names (`character(n)`) if provided, returns the code only for objects specified in `names`.
#' @return If `deparse = TRUE`, a `character` with the reproducible code. For `deparse = FALSE`, an expression with the
#' code.
#' @examples
#' q1 <- new_qenv(env = list2env(list(a = 1)), code = quote(a <- 1))
#' q2 <- eval_code(q1, code = quote(b <- a))
#' q3 <- eval_code(q2, code = quote(d <- 2))
#' get_code(q3)
#' get_code(q3, deparse = FALSE)
#' @export
setGeneric("get_code", function(object, deparse = TRUE, names = character(0)) {
  # this line forces evaluation of object before passing to the generic
  # needed for error handling to work properly
  grDevices::pdf(nullfile())
  on.exit(grDevices::dev.off())
  object

  standardGeneric("get_code")
})

#' @rdname get_code
#' @export
setMethod("get_code", signature = "qenv", function(object, deparse = TRUE, names = character(0)) {
  checkmate::assert_character(names)
  checkmate::assert_flag(deparse)
  code <- if (length(names) > 0) {
    get_code_dependency(object, names)
  } else {
    object@code
  }
  if (deparse) {
    format_expression(code)
  } else {
    parse(text = code)
  }
})

#' @rdname get_code
#' @export
setMethod("get_code", signature = "qenv.error", function(object) {
  stop(
    errorCondition(
      sprintf(
        "%s\n\ntrace: \n %s\n",
        conditionMessage(object),
        paste(object$trace, collapse = "\n ")
      ),
      class = c("validation", "try-error", "simpleError")
    )
  )
})
