#' Show the `qenv` object
#'
#' Prints the `qenv` object
#' @param object (`qenv`)
#' @return nothing
#' @importFrom methods show
#' @examples
#' q1 <- new_qenv(
#'   code = "a <- 5
#'     b <- data.frame(x = 1:10)",
#'   env = list2env(list(a = 5, b = data.frame(x = 1:10)))
#' )
#' q1
#' @export
setMethod("show", "qenv", function(object) {
  rlang::env_print(object@env)
})
