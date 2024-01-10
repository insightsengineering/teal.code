#' Display `qenv` object
#'
#' Prints the `qenv` object.
#'
#' @param object (`qenv`)
#'
#' @return `object`, invisibly.
#'
#' @examples
#' q <- qenv()
#' q1 <- eval_code(q, expression(a <- 5, b <- data.frame(x = 1:10)))
#' q1
#'
#' @aliases show-qenv
#'
#' @importFrom methods show
#' @export
setMethod("show", "qenv", function(object) {
  rlang::env_print(object@env)
})
