#' Get code from `qenv`
#'
#' @name get_code
#' @param object (`qenv`)
#' @param parse (`logical(1)`) if the returned code should be converted to expression.
#' @param names (`character(n)`) if provided, returns the code only for objects specified in `names`.
#' @return named `character` with the reproducible code.
#' @examples
#' q1 <- new_qenv(env = list2env(list(a = 1)), code = quote(a <- 1))
#' q2 <- eval_code(q1, code = quote(b <- a))
#' q3 <- eval_code(q2, code = quote(d <- 2))
#' get_code(q3)
#' get_code(q3, deparse = FALSE)
#' @export
setGeneric("get_code", function(object, parse = FALSE, names = NULL) {
  # this line forces evaluation of object before passing to the generic
  # needed for error handling to work properly
  grDevices::pdf(nullfile())
  on.exit(grDevices::dev.off())
  object

  standardGeneric("get_code")
})

#' @rdname get_code
#' @export
setMethod("get_code", signature = "qenv", function(object, parse = FALSE, names = NULL) {
  checkmate::assert_flag(parse)
  code <- if (!is.null(names)) {
    get_code_dependency(object, names)
  } else {
    object@code
  }
  if (parse) {
    parse(text = code)
  } else {
    code
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
