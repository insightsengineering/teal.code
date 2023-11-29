#' Get code from `qenv`
#'
#' @name get_code
#' @param object (`qenv`)
#' @param deparse (`logical(1)`) if the returned code should be converted to character.
#' @param ... arguments passed to methods.
#' @return named `character` with the reproducible code.
#' @examples
#' q1 <- new_qenv(env = list2env(list(a = 1)), code = quote(a <- 1))
#' q2 <- eval_code(q1, code = quote(b <- a))
#' q3 <- eval_code(q2, code = quote(d <- 2))
#' get_code(q3)
#' get_code(q3, deparse = FALSE)
#' @export
setGeneric("get_code", function(object, deparse = TRUE, ...) {
  # this line forces evaluation of object before passing to the generic
  # needed for error handling to work properly
  grDevices::pdf(nullfile())
  on.exit(grDevices::dev.off())
  object

  standardGeneric("get_code")
})

#' @rdname get_code
#' @export
setMethod("get_code", signature = "qenv", function(object, deparse = TRUE) {
  checkmate::assert_flag(deparse)
  if (deparse) {
    if (length(object@code) == 0) {
      object@code
    } else {
      paste(object@code, collapse = "\n")
    }
  } else {
    parse(text = paste(c("{", object@code, "}"), collapse = "\n"), keep.source = TRUE)
  }
})

#' @rdname get_code
#' @export
setMethod("get_code", signature = "qenv.error", function(object, ...) {
  stop(
    errorCondition(
      sprintf(
        "%s\n\ntrace: \n %s\n",
        conditionMessage(object),
        paste(lang2calls(object$trace), collapse = "\n ")
      ),
      class = c("validation", "try-error", "simpleError")
    )
  )
})
