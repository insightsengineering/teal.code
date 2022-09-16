#' Join two `Quosure` objects
#'
#' Combine two `Quosure` object by merging their environments and the code.
#' Not every `Quosure` objects can be combined:
#' - if their environments contains objects of the same name but not identical
#' - if `y` has unique code element placed before common element. This means that `y`
#' in the environment of the `y` was evaluated some extra code before which can influence
#' reproducibility
#' - more cases to be done
#' @param x (`Quosure`)
#' @param y (`Quosure`)
#' @examples
#' q1 <- new_quosure(
#'   code = c(iris1 = "iris1 <- iris", mtcars1 = "mtcars1 <- mtcars"),
#'   env = list2env(list(
#'     iris1 = iris,
#'     mtcars1 = mtcars
#'   ))
#' )
#' q2 <- q1
#' q1 <- eval_code(q1, "iris2 <- iris")
#' q2 <- eval_code(q2, "mtcars2 <- mtcars")
#' qq <- join(q1, q2)
#' get_code(qq)
#' @export
setGeneric("join", function(x, y) {
  standardGeneric("join")
})

#' @rdname join
#' @export
setMethod("join", signature = c("Quosure", "Quosure"), function(x, y) {
  join_validation <- check_joinable(x, y)

  # join expressions
  if (!isTRUE(join_validation)) {
    stop(join_validation)
  }

  id_unique <- !y@id %in% x@id
  x@id <- c(x@id, y@id[id_unique])
  x@code <- c(x@code, y@code[id_unique])

  # insert (and overwrite) objects from env2 to env
  x@env <- rlang::env_clone(x@env, parent = parent.env(.GlobalEnv))
  rlang::env_coalesce(env = x@env, from = y@env)
  x
})

#' @rdname join
#' @export
setMethod("join", signature = "quosure.error", function(x, y) {
  x
})

#' @rdname join
#' @export
setMethod("join", signature = c("Quosure", "quosure.error"), function(x, y) {
  y
})

#' If two `Quosure` can be joined
#'
#' Checks if two `Quosure` objects can be combined.
#' They can't be combined if (and):
#' - both share the same code (identified by `id`)
#' - indices of the shared code are not consecutive or don't start from 1
#' @param x (`Quosure`)
#' @param y (`Quosure`)
#' @return `TRUE` if able to join or `character` used to print error message.
#' @keywords internal
check_joinable <- function(x, y) {
  checkmate::assert_class(x, "Quosure")
  checkmate::assert_class(y, "Quosure")

  common_names <- intersect(ls(x@env), ls(y@env))
  is_overwritten <- vapply(common_names, function(el) {
    !identical(get(el, x@env), get(el, y@env))
  }, logical(1))
  if (any(is_overwritten)) {
    return(
      paste(
        "Not possible to join Quosure objects if anything in their environment has been modified.\n",
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
      "The common code of joined objects doesn't have the same indices. It means that `x` and `y`",
      "can't be joined together as it's impossible to determine the evaluation's order.",
      collapse = ""
    )
  } else {
    paste(
      "The common code of joined object does not start from index = 1.",
      "It means that joined object(x) has some extra code elements before.",
      collapse = ""
    )
  }
}
