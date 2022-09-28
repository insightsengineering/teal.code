#' Get the warnings of qenv object
#'
#' @param object (`qenv`)
#'
#' @return `character` containing warning information or `NULL` if no warnings
#' @export
#'
#' @examples
#' data_q <- new_qenv() %>% eval_code("iris_data <- iris")
#' warning_qenv <- eval_code(
#'   data_q,
#'   bquote(p <- hist(iris_data[, .("Sepal.Length")], ff = ""))
#' )
#' cat(get_warnings(warning_qenv))
#' @export
setGeneric("get_warnings", function(object) {
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

  warning_output <- "Warnings:"
  for(warn_idx in seq_along(object@warnings)) {
    warn <- object@warnings[warn_idx]
    if (warn != "") {
      warning_output <- paste(warn, "\nWhen running code:\n", paste(object@code[warn_idx], collapse = "\n"))
    }
  }
  paste(warning_output, "\n\nTrace:\n", paste(get_code(object), collapse = "\n"))
})
