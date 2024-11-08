#' Get messages from `qenv` object
#'
#' Retrieve all messages raised during code evaluation in a `qenv`.
#'
#' @param object (`qenv`)
#'
#' @return `character` containing warning information or `NULL` if no messages.
#'
#' @examples
#' data_q <- qenv()
#' data_q <- eval_code(data_q, "iris_data <- iris")
#' warning_qenv <- eval_code(
#'   data_q,
#'   bquote(p <- hist(iris_data[, .("Sepal.Length")], ff = ""))
#' )
#' cat(get_messages(warning_qenv))
#'
#' @name get_messages
#' @rdname get_messages
#' @aliases get_messages,qenv-method
#' @aliases get_messages,qenv.error-method
#' @aliases get_messages,NULL-method
#'
#' @export
setGeneric("get_messages", function(object) {
  dev_suppress(object)
  standardGeneric("get_messages")
})

setMethod("get_messages", signature = "qenv", function(object) {
  get_warn_message_util(object, "message")
})

setMethod("get_messages", signature = "qenv.error", function(object) {
  NULL
})

setMethod("get_messages", "NULL", function(object) {
  NULL
})
