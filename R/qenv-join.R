#' Join `qenv` objects
#'
#' @description
#' Checks and merges two `qenv` objects into one `qenv` object.
#'
#' The `join()` function is superseded by the `c()` function.
#'
#' @details
#' Any common code at the start of the `qenvs` is only placed once at the start of the joined `qenv`.
#' This allows consistent behavior when joining `qenvs` which share a common ancestor.
#' See below for an example.
#'
#' There are some situations where `join()` cannot be properly performed, such as these three scenarios:
#' 1. Both `qenv` objects contain an object of the same name but are not identical.
#'
#'    Example:
#'
#'    ```r
#'    x <- eval_code(qenv(), expression(mtcars1 <- mtcars))
#'    y <- eval_code(qenv(), expression(mtcars1 <- mtcars['wt']))
#'
#'    z <- c(x, y)
#'    # Error message will occur
#'    ```
#'    In this example, `mtcars1` object exists in both `x` and `y` objects but the content are not identical.
#'    `mtcars1` in the `x qenv` object has more columns than `mtcars1` in the `y qenv` object (only has one column).
#'
#' 2. `join()` will look for identical code elements in both `qenv` objects.
#'    The index position of these code elements must be the same to determine the evaluation order.
#'    Otherwise, `join()` will throw an error message.
#'
#'    Example:
#'    ```r
#'    common_q <- eval_code(qenv(), expression(v <- 1))
#'    x <- eval_code(
#'      common_q,
#'      "x <- v"
#'    )
#'    y <- eval_code(
#'      common_q,
#'      "y <- v"
#'    )
#'    z <- eval_code(
#'      y,
#'      "z <- v"
#'    )
#'    q <- c(x, y)
#'    join_q <- c(q, z)
#'    # Error message will occur
#'
#'    # Check the order of evaluation based on the id slot
#'    ```
#'    The error occurs because the index position of common code elements in the two objects is not the same.
#'
#' 3. The usage of temporary variable in the code expression could cause `join()` to fail.
#'
#'    Example:
#'    ```r
#'    common_q <- qenv()
#'    x <- eval_code(
#'      common_q,
#'      "x <- numeric(0)
#'       for (i in 1:2) {
#'         x <- c(x, i)
#'       }"
#'    )
#'    y <- eval_code(
#'      common_q,
#'      "y <- numeric(0)
#'       for (i in 1:3) {
#'         y <- c(y, i)
#'       }"
#'    )
#'    q <- join(x,y)
#'    # Error message will occur
#'
#'    # Check the value of temporary variable i in both objects
#'    x$i # Output: 2
#'    y$i # Output: 3
#'    ```
#'    `c()` fails to provide a proper result because of the temporary variable `i` exists
#'    in both objects but has different value.
#'    To fix this, we can set `i <- NULL` in the code expression for both objects.
#'    ```r
#'    common_q <- qenv()
#'    x <- eval_code(
#'      common_q,
#'      "x <- numeric(0)
#'       for (i in 1:2) {
#'         x <- c(x, i)
#'       }
#'       # dummy i variable to fix it
#'       i <- NULL"
#'    )
#'    y <- eval_code(
#'      common_q,
#'      "y <- numeric(0)
#'       for (i in 1:3) {
#'         y <- c(y, i)
#'       }
#'       # dummy i variable to fix it
#'       i <- NULL"
#'    )
#'    q <- c(x,y)
#'    ```
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
#' qq <- join(q1, q2)
#' cat(get_code(qq))
#'
#' common_q <- eval_code(q, quote(x <- 1))
#' y_q <- eval_code(common_q, quote(y <- x * 2))
#' z_q <- eval_code(common_q, quote(z <- x * 3))
#' join_q <- join(y_q, z_q)
#' # get_code only has "x <- 1" occurring once
#' cat(get_code(join_q))
#'
#' @include qenv-errors.R
#'
#' @name join
#' @rdname join
#' @aliases join,qenv,qenv-method
#' @aliases join,qenv,qenv.error-method
#' @aliases join,qenv.error,ANY-method
#'
#' @export
setGeneric("join", function(x, y) standardGeneric("join"))

setMethod("join", signature = c("qenv", "qenv"), function(x, y) {
  lifecycle::deprecate_soft("0.6.0", "join()", "c()")
  c(x, y)
})

setMethod("join", signature = c("qenv", "qenv.error"), function(x, y) {
  lifecycle::deprecate_soft("0.6.0", "join()", "c()")
  y
})

setMethod("join", signature = c("qenv.error", "ANY"), function(x, y) {
  lifecycle::deprecate_soft("0.6.0", "join()", "c()")
  x
})
