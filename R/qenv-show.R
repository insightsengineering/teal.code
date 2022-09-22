#' Show the `qenv` object
#'
#' Prints the `qenv` object
#' @param object (`qenv`)
#' @return nothing
#' @importFrom methods show
#' @examples
#' q1 <- new_qenv(code = "print('a')", env = new.env())
#' q1
#' @export
setMethod("show", "qenv", function(object) {
  obs <- names(as.list(object@env))
  if (length(obs) > 0) {
    cat(sprintf("A qenv object containing: %s\n", paste(obs, collapse = ", ")))
  } else {
    cat("A qenv object containing no objects.\n")
  }
})
