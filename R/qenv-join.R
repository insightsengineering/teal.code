#' Join two `qenv` objects
#'
#' `join()` perform checks and merges two `qenv` objects into one `qenv` object.
#' Any common code at the start of the `qenvs` is only placed once at the start of the joined `qenv`.
#' This allows consistent behavior when joining `qenvs` which share a common ancestor.
#' See below for an example.
#'
#' There are some situations where `join()` cannot be properly performed, such as these three scenarios:
#' \enumerate{
#'   \item Both `qenv` objects contain an object of the same name but are not identical. \cr\cr
#'   Example:
#'   \preformatted{
#'   x <- qenv(
#'     code = c(mtcars1 = "mtcars1 <- mtcars"),
#'     env = list2env(list(mtcars1 = mtcars))
#'   )
#'   y <- qenv(
#'     code = c(mtcars1 = "mtcars1 <- mtcars['wt']"),
#'     env = list2env(list(mtcars1 = mtcars['wt']))
#'   )
#'   z <- join(x, y)
#'   # Error message will occur
#'   }
#'   In this example, `mtcars1` object exists in both `x` and `y` objects but the content are not identical.\cr
#'   `mtcars1` in the `x qenv` object has more columns than `mtcars1` in the `y qenv` object (only has one column).
#'   \item `join()` will look for identical `@id` values in both `qenv` objects.
#'   The index position of these `@id`s must be the same to determine the evaluation order.
#'   Otherwise, `join()` will throw an error message.\cr\cr
#'   Example:
#'   \preformatted{
#'   common_q <- qenv(code = "v <- 1", env = list2env(list(v = 1)))
#'   x <- eval_code(
#'     common_q,
#'     "x <- v"
#'   )
#'   y <- eval_code(
#'     common_q,
#'     "y <- v"
#'   )
#'   z <- eval_code(
#'     y,
#'     "z <- v"
#'   )
#'   q <- join(x, y)
#'   join_q <- join(q, z)
#'   # Error message will occur
#'
#'   # Check the order of evaluation based on the id slot
#'   shared_ids <- intersect(q@id, z@id)
#'   match(shared_ids, q@id) # Output: 1 3
#'   match(shared_ids, z@id) # Output: 1 2
#'   }
#'   The error occurs because the index position of identical `@id` between the two objects is not the same.
#'   \item The usage of temporary variable in the code expression could cause `join()` to fail. \cr\cr
#'   Example:
#'   \preformatted{
#'   common_q <- qenv()
#'   x <- eval_code(
#'     common_q,
#'     "x <- numeric(0)
#'      for (i in 1:2) {
#'        x <- c(x, i)
#'      }"
#'   )
#'   y <- eval_code(
#'     common_q,
#'     "y <- numeric(0)
#'      for (i in 1:3) {
#'        y <- c(y, i)
#'      }"
#'   )
#'   q <- join(x,y)
#'   # Error message will occur
#'
#'   # Check the value of temporary variable i in both objects
#'   x@env$i # Output: 2
#'   y@env$i # Output: 3
#'   }
#'   `join()` fails to provide a proper result because of the temporary variable `i` exists
#'   in both objects but has different value.\cr
#'   To fix this, we can set `i <- NULL` in the code expression for both objects.
#'   \preformatted{
#'   common_q <- qenv()
#'   x <- eval_code(
#'     common_q,
#'     "x <- numeric(0)
#'      for (i in 1:2) {
#'        x <- c(x, i)
#'      }
#'      # dummy i variable to fix it
#'      i <- NULL"
#'   )
#'   y <- eval_code(
#'     common_q,
#'     "y <- numeric(0)
#'      for (i in 1:3) {
#'        y <- c(y, i)
#'      }
#'      # dummy i variable to fix it
#'      i <- NULL"
#'   )
#'   q <- join(x,y)
#'   }
#' }
#'
#' @param x (`qenv`)
#' @param y (`qenv`)
#' @include qenv-errors.R
#' @return `qenv` object.
#' @examples
#' q <- qenv()
#' q1 <- eval_code(q, expression(iris1 <- iris, mtcars1 <- mtcars))
#' q2 <- q1
#' q1 <- eval_code(q1, "iris2 <- iris")
#' q2 <- eval_code(q2, "mtcars2 <- mtcars")
#' qq <- join(q1, q2)
#' get_code(qq)
#'
#' common_q <- eval_code(q, quote(x <- 1))
#' y_q <- eval_code(common_q, quote(y <- x * 2))
#' z_q <- eval_code(common_q, quote(z <- x * 3))
#' join_q <- join(y_q, z_q)
#' # get_code only has "x <- 1" occurring once
#' get_code(join_q)
#'
#' @export
setGeneric("join", function(x, y) standardGeneric("join"))

#' @rdname join
#' @export
setMethod("join", signature = c("qenv", "qenv"), function(x, y) {
  join_validation <- .check_joinable(x, y)

  # join expressions
  if (!isTRUE(join_validation)) {
    stop(join_validation)
  }

  id_unique <- !y@id %in% x@id
  x@id <- c(x@id, y@id[id_unique])
  x@code <- c(x@code, y@code[id_unique])
  x@warnings <- c(x@warnings, y@warnings[id_unique])
  x@messages <- c(x@messages, y@messages[id_unique])

  # insert (and overwrite) objects from y to x
  x@env <- rlang::env_clone(x@env, parent = parent.env(.GlobalEnv))
  rlang::env_coalesce(env = x@env, from = y@env)
  x
})

#' @rdname join
#' @export
setMethod("join", signature = "qenv.error", function(x, y) {
  x
})

#' @rdname join
#' @export
setMethod("join", signature = c("qenv", "qenv.error"), function(x, y) {
  y
})

#' If two `qenv` can be joined
#'
#' Checks if two `qenv` objects can be combined.
#' For more information, please see \code{\link{join}}
#' @param x (`qenv`)
#' @param y (`qenv`)
#' @return `TRUE` if able to join or `character` used to print error message.
#' @keywords internal
.check_joinable <- function(x, y) {
  checkmate::assert_class(x, "qenv")
  checkmate::assert_class(y, "qenv")

  common_names <- intersect(rlang::env_names(x@env), rlang::env_names(y@env))
  is_overwritten <- vapply(common_names, function(el) {
    !identical(get(el, x@env), get(el, y@env))
  }, logical(1))
  if (any(is_overwritten)) {
    return(
      paste(
        "Not possible to join qenv objects if anything in their environment has been modified.\n",
        "Following object(s) have been modified:\n - ",
        paste(common_names[is_overwritten], collapse = "\n - ")
      )
    )
  }

  shared_ids <- intersect(x@id, y@id)
  if (length(shared_ids) == 0) {
    return(TRUE)
  }

  shared_in_x <- match(shared_ids, x@id)
  shared_in_y <- match(shared_ids, y@id)

  # indices of shared ids should be 1:n in both slots
  if (identical(shared_in_x, shared_in_y) && identical(shared_in_x, seq_along(shared_ids))) {
    TRUE
  } else if (!identical(shared_in_x, shared_in_y)) {
    paste(
      "The common shared code of the qenvs does not occur in the same position in both qenv objects",
      "so they cannot be joined together as it's impossible to determine the evaluation's order.",
      collapse = ""
    )
  } else {
    paste(
      "There is code in the qenv objects before their common shared code",
      "which means these objects cannot be joined.",
      collapse = ""
    )
  }
}
