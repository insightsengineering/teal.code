#' If two `qenv` can be joined
#'
#' Checks if two `qenv` objects can be combined.
#' For more information, please see [`join`]
#' @param x (`qenv`)
#' @param y (`qenv`)
#' @return `TRUE` if able to join or `character` used to print error message.
#' @keywords internal
.check_joinable <- function(x, y) {
  checkmate::assert_class(x, "qenv")
  checkmate::assert_class(y, "qenv")

  common_names <- intersect(rlang::env_names(x@.xData), rlang::env_names(y@.xData))
  is_overwritten <- vapply(common_names, function(el) {
    !identical(get(el, x@.xData), get(el, y@.xData))
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

  x_id <- names(x@code)
  y_id <- names(y@code)

  shared_ids <- intersect(x_id, y_id)
  if (length(shared_ids) == 0) {
    return(TRUE)
  }

  shared_in_x <- match(shared_ids, x_id)
  shared_in_y <- match(shared_ids, y_id)

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

#' @rdname join
#' @param ... (`qenv` or `qenv.error`).
#' @examples
#' q <- qenv()
#' q1 <- within(q, {
#'   iris1 <- iris
#'   mtcars1 <- mtcars
#' })
#' q1 <- within(q1, iris2 <- iris)
#' q2 <- within(q1, mtcars2 <- mtcars)
#' qq <- c(q1, q2)
#' cat(get_code(qq))
#'
#' @export
c.qenv <- function(...) {
  dots <- rlang::list2(...)
  if (!checkmate::test_list(dots[-1], types = c("qenv", "qenv.error"))) {
    return(NextMethod(c, dots[[1]]))
  }

  first_non_qenv_ix <- which.min(vapply(dots, inherits, what = "qenv", logical(1)))
  if (first_non_qenv_ix > 1) {
    return(dots[[first_non_qenv_ix]])
  }

  Reduce(
    x = dots[-1],
    init = dots[[1]],
    f = function(x, y) {
      join_validation <- .check_joinable(x, y)

      # join expressions
      if (!isTRUE(join_validation)) {
        stop(join_validation)
      }

      x@code <- utils::modifyList(x@code, y@code)

      # insert (and overwrite) objects from y to x
      x@.xData <- rlang::env_clone(x@.xData, parent = parent.env(.GlobalEnv))
      rlang::env_coalesce(env = x@.xData, from = y@.xData)
      x
    }
  )
}

#' @rdname join
#' @export
c.qenv.error <- function(...) {
  rlang::list2(...)[[1]]
}
