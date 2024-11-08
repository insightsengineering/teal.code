#' Get warnings from `qenv` object
#'
#' Retrieve all warnings raised during code evaluation in a `qenv`.
#'
#' @param object (`qenv`)
#'
#' @return `character` containing warning information or `NULL` if no warnings.
#'
#' @examples
#' data_q <- qenv()
#' data_q <- eval_code(data_q, "iris_data <- iris")
#' warning_qenv <- eval_code(
#'   data_q,
#'   bquote(p <- hist(iris_data[, .("Sepal.Length")], ff = ""))
#' )
#' cat(get_warnings(warning_qenv))
#'
#' @name get_warnings
#' @rdname get_warnings
#' @aliases get_warnings,qenv-method
#' @aliases get_warnings,qenv.error-method
#' @aliases get_warnings,NULL-method
#'
#' @export
setGeneric("get_warnings", function(object) {
  dev_suppress(object)
  standardGeneric("get_warnings")
})

setMethod("get_warnings", signature = "qenv", function(object) {
  get_warn_message_util(object, "warning")
})

setMethod("get_warnings", signature = "qenv.error", function(object) {
  NULL
})

setMethod("get_warnings", "NULL", function(object) {
  NULL
})
