#' Show the `Quosure` object
#'
#' Prints the `Quosure` object
#' @param object (`Quosure`)
#' @return nothing
#' @importFrom methods show
#' @examples
#' q1 <- new_quosure(code = "print('a')", env = new.env())
#' q1
#' @export
setMethod("show", "Quosure", function(object) {
  obs <- names(as.list(object@env))
  if (length(obs) > 0) {
    cat(paste("A quosure object containing:", paste(obs, collapse = ", ")))
  } else {
    cat("A quosure object containing no objects")
  }
})


