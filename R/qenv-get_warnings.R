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
  # this line forces evaluation of object before passing to the generic
  # needed for error handling to work properly
  grDevices::pdf(nullfile())
  on.exit(grDevices::dev.off())
  object

  standardGeneric("get_warnings")
})

setMethod("get_warnings", signature = c("qenv"), function(object) {
  if (all(object@warnings == "")) {
    return(NULL)
  }

  lines <- mapply(
    function(warn, expr) {
      if (warn == "") {
        return(NULL)
      }
      sprintf("%swhen running code:\n%s", warn, paste(lang2calls(expr), collapse = "\n"))
    },
    warn = as.list(object@warnings),
    expr = as.list(as.character(object@code))
  )
  lines <- Filter(Negate(is.null), lines)

  sprintf(
    "~~~ Warnings ~~~\n\n%s\n\n~~~ Trace ~~~\n\n%s",
    paste(lines, collapse = "\n\n"),
    get_code(object)
  )
})

setMethod("get_warnings", signature = c("qenv.error"), function(object) {
  NULL
})

setMethod("get_warnings", "NULL", function(object) {
  NULL
})
