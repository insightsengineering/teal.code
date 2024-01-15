#' Concatenate two `qenv` objects
#'
#' Combine two `qenv` objects by simple concatenate their environments and the code.
#'
#' We recommend to use the `join` method to have a stricter control
#' in case `x` and `y` contain duplicated bindings and code.
#' RHS argument content has priority over the LHS one.
#'
#' @param x (`qenv`)
#' @param y (`qenv`)
#'
#' @return `qenv` object.
#'
#' @examples
#' q <- qenv()
#' q1 <- eval_code(q, expression(iris1 <- iris, mtcars1 <- mtcars))
#' q2 <- q1
#' q1 <- eval_code(q1, "iris2 <- iris")
#' q2 <- eval_code(q2, "mtcars2 <- mtcars")
#' qq <- concat(q1, q2)
#' get_code(qq)
#'
#' @include qenv-errors.R
#'
#' @name concat
#' @rdname concat
#' @aliases concat,qenv,qenv-method
#' @aliases concat,qenv.error,ANY-method
#' @aliases concat,qenv,qenv.error-method
#'
#' @export
setGeneric("concat", function(x, y) standardGeneric("concat"))

setMethod("concat", signature = c("qenv", "qenv"), function(x, y) {
  y@id <- c(x@id, y@id)
  y@code <- c(x@code, y@code)
  y@warnings <- c(x@warnings, y@warnings)
  y@messages <- c(x@messages, y@messages)

  # insert (and overwrite) objects from y to x
  y@env <- rlang::env_clone(y@env, parent = parent.env(.GlobalEnv))
  rlang::env_coalesce(env = y@env, from = x@env)
  y
})

setMethod("concat", signature = c("qenv.error", "ANY"), function(x, y) {
  x
})

setMethod("concat", signature = c("qenv", "qenv.error"), function(x, y) {
  y
})
