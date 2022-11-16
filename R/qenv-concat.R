#' Concatenate two `qenv` objects
#'
#' Combine two `qenv` objects by simple concatenate their environments and the code.
#' We recommend to use the `join` method to have a stricter control 
#' in case `x` and `y` contain duplicated bindings and code.
#' RHS argument content has priority over the LHS one.
#' @param x (`qenv`)
#' @param y (`qenv`)
#' @include qenv-errors.R
#' @examples
#' q1 <- new_qenv(
#'   code = c(iris1 = "iris1 <- iris", mtcars1 = "mtcars1 <- mtcars"),
#'   env = list2env(list(
#'     iris1 = iris,
#'     mtcars1 = mtcars
#'   ))
#' )
#' q2 <- q1
#' q1 <- eval_code(q1, "iris2 <- iris")
#' q2 <- eval_code(q2, "mtcars2 <- mtcars")
#' qq <- concat(q1, q2)
#' get_code(qq)
#' @export
setGeneric("concat", function(x, y) {
  standardGeneric("concat")
})

#' @rdname concat
#' @export
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

#' @rdname concat
#' @export
setMethod("concat", signature = c("qenv.error"), function(x, y) {
  x
})

#' @rdname concat
#' @export
setMethod("concat", signature = c("qenv", "qenv.error"), function(x, y) {
  y
})
