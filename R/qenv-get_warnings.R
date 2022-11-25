#' Get the warnings of `qenv` object
#'
#' @param object (`qenv`)
#'
#' @return `character` containing warning information or `NULL` if no warnings
#' @export
#'
#' @examples
#' data_q <- new_qenv()
#' data_q <- eval_code(new_qenv(), "iris_data <- iris")
#' warning_qenv <- eval_code(
#'   data_q,
#'   bquote(p <- hist(iris_data[, .("Sepal.Length")], ff = ""))
#' )
#' cat(get_warnings(warning_qenv))
#' @export
setGeneric("get_warnings", function(object) {
  # this line forces evaluation of object before passing to the generic
  # needed for error handling to work properly
  object

  standardGeneric("get_warnings")
})

#' @rdname get_warnings
#' @export
setMethod("get_warnings", signature = c("qenv.error"), function(object) {
  NULL
})

#' @rdname get_warnings
#' @export
setMethod("get_warnings", signature = c("qenv"), function(object) {
  if (all(object@warnings == "")) {
    return(NULL)
  }

  lines <- mapply(
    function(warn, expr) {
      if (warn == "") {
        return(NULL)
      }
      sprintf("%swhen running code:\n%s", warn, paste(format_expression(expr), collapse = "\n"))
    },
    warn = as.list(object@warnings),
    expr = as.list(as.character(object@code))
  )
  lines <- Filter(Negate(is.null), lines)

  sprintf(
    "~~~ Warnings ~~~\n\n%s\n\n~~~ Trace ~~~\n\n%s",
    paste(lines, collapse = "\n\n"),
    paste(get_code(object), collapse = "\n")
  )
})

#' @rdname get_warnings
#' @export
setMethod("get_warnings", "NULL", function(object) {
  NULL
})
