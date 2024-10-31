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
  warnings <- lapply(object@code, "attr", "warning")
  code <- object@code[unlist(lapply(warnings, Negate(is.null)))]
  if (length(unlist(warnings)) == 0) {
    return(NULL)
  }

  lines <- mapply(
    function(warn, expr) {
      if (warn == "") {
        return(NULL)
      }
      sprintf("%swhen running code:\n%s", warn, paste(lang2calls(expr), collapse = "\n"))
    },
    warn = as.list(unlist(warnings)),
    expr = as.list(unlist(code))
  )
  lines <- Filter(Negate(is.null), lines)

  paste0(
    sprintf("~~~ Warnings ~~~\n\n%s\n\n", paste(lines, collapse = "\n\n")),
    sprintf("~~~ Trace ~~~\n\n%s", paste(get_code(object), collapse = "\n"))
  )
})

setMethod("get_warnings", signature = c("qenv.error"), function(object) {
  NULL
})

setMethod("get_warnings", "NULL", function(object) {
  NULL
})
