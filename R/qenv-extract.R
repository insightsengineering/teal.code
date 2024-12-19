#' Subsets `qenv`
#'
#' @description
#' Subsets [`qenv`] environment and limits the code to the necessary needed to build limited objects.
#'
#' @param x (`qenv`)
#' @param names (`character`) names of objects included in [`qenv`] to subset. Names not present in [`qenv`]
#' are skipped.
#' @param ... internal usage, please ignore.
#'
#' @name subset-qenv
#'
#' @examples
#' q <- qenv()
#' q <- eval_code(q, "a <- 1;b<-2")
#' q["a"]
#' q[c("a", "b")]
#'
#' @export
`[.qenv` <- function(x, names, ...) {
  checkmate::assert_character(names, any.missing = FALSE)
  possible_names <- ls(get_env(x), all.names = TRUE)
  names_corrected <- intersect(names, possible_names)
  env <- if (length(names_corrected)) {
    names_missing <- setdiff(names, possible_names)
    if (length(names_missing)) {
      warning(
        sprintf(
          "Some 'names' do not exist in the environment of the '%s'. Skipping those: %s.",
          class(x)[1],
          paste(names_missing, collapse = ", ")
        )
      )
    }
    list2env(as.list(x, all.names = TRUE)[names_corrected], parent = parent.env(.GlobalEnv))
  } else {
    warning(
      sprintf(
        "None of 'names' exist in the environment of the '%1$s'. Returning empty '%1$s'.",
        class(x)[1]
      ),
      call. = FALSE
    )
    new.env(parent = parent.env(.GlobalEnv))
  }
  lockEnvironment(env)
  x@.xData <- env

  normalized_names <- gsub("^`(.*)`$", "\\1", names)
  x@code <- get_code_dependency(x@code, names = normalized_names, ...)

  x
}
