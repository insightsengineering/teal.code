#'
#' @section Subsetting:
#' `x[names]` subsets objects in `qenv` environment and limit the code to the necessary needed to build limited objects.
#' `...` passes parameters to further methods.
#'
#' @param x (`qenv`)
#'
#' @examples
#'
#' # Subsetting
#' q <- qenv()
#' q <- eval_code(q, "a <- 1;b<-2")
#' q["a"]
#' q[c("a", "b")]
#'
#' @rdname qenv
#'
#' @export
`[.qenv` <- function(x, names, ...) {
  checkmate::assert_class(names, "character")
  possible_names <- ls(get_env(x), all.names = TRUE)
  names_warn <- setdiff(names, possible_names)
  names <- intersect(names, possible_names)
  if (!length(names)) {
    warning(
      sprintf(
        "None of 'names' elements exist in '%s'. Returning empty '%s'.",
        class(x)[1],
        class(x)[1]
      )
    )
    return(qenv())
  }

  if (length(names_warn)) {
    warning(
      sprintf(
        "Some elements of 'names' do not exist in '%s'. Skipping those: %s.",
        class(x)[1],
        paste(names_warn, collapse = ", ")
      )
    )
  }

  x@env <- list2env(mget(x = names, envir = get_env(x)))
  names <- gsub("^`(.*)`$", "\\1", names)
  x@code <- get_code_dependency(x@code, names = names, ...)

  x
}
