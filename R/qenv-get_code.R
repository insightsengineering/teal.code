#' Get Code From `qenv`
#'
#' @details
#' `get_code` retrieves the code stored in the `qenv`. `...` passes arguments to methods.
#'
#' @param object (`qenv`)
#' @param deparse (`logical(1)`) flag specifying whether to return code as `character` or `expression`.
#' @param ... see `Details`
#'
#' @return
#' `get_code` returns the traced code (from `@code` slot) in the form specified by `deparse`.
#'
#' @examples
#' # retrieve code
#' get_code(q)
#' get_code(q, deparse = FALSE)
#'
#' @name get_code
#' @rdname qenv
#' @aliases get_code,qenv-method
#' @aliases get_code,qenv.error-method
#'
#' @export
setGeneric("get_code", function(object, deparse = TRUE, ...) {
  # this line forces evaluation of object before passing to the generic
  # needed for error handling to work properly
  grDevices::pdf(nullfile())
  on.exit(grDevices::dev.off())
  object

  standardGeneric("get_code")
})

setMethod("get_code", signature = "qenv", function(object, deparse = TRUE) {
  checkmate::assert_flag(deparse)
  if (deparse) {
    object@code
  } else {
    parse(text = object@code, keep.source = TRUE)
  }
})

setMethod("get_code", signature = "qenv.error", function(object, ...) {
  stop(
    errorCondition(
      sprintf(
        "%s\n\ntrace: \n %s\n",
        conditionMessage(object),
        paste(format_expression(object$trace), collapse = "\n ")
      ),
      class = c("validation", "try-error", "simpleError")
    )
  )
})
