#'
#' @section Subsetting:
#' `x[names]` subsets objects in `qenv` environment and limit the code to the necessary needed to build limited objects.
#'
#' @param names (`character`) names of objects included in `qenv` to subset
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
`[.qenv` <- function(x, names) {
  checkmate::assert_class(names, "character")
  possible_names <- ls(get_env(x))
  names_warn <- setdiff(names, possible_names)
  names <- intersect(names, possible_names)
  if (!length(names)) {
    warning("None of `names` elements exist in `qenv`. Returning empty `qenv`.")
    return(qenv())
  }

  if (length(names_warn)) {
    warning(
      sprintf(
        "Some elements of `names` do not exist in `qenv`. Skipping those: %s.",
        paste(names_warn, collapse = ", ")
      )
    )
  }

  limited_code <- get_code(x, names = names)
  indexes <- which(x@code %in% limited_code)

  x@env <- list2env(mget(x = names, envir = get_env(x)))
  x@code <- limited_code
  x@id <- x@id[indexes]
  x@warnings <- x@warnings[indexes]
  x@messages <- x@messages[indexes]

  x
}
