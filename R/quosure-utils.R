#' Copy environment content to the other environment
#' To avoid sharing the same environment by multiple `Quosure` objects
#' @keywords internal
.copy_env <- function(env) {
  # need to copy the objects from old env to new env
  # to avoid quosure objects sharing the same environment
  env_new <- new.env(parent = parent.env(.GlobalEnv))
  for (i in ls(env)) {
    env_new[[i]] <- env[[i]]
  }
  env_new
}

#' Combine two code vectors and keep unique names
#'
#' Combines two code vectors and modifies duplicated names.
#' @param x (`character`) vector of code.
#' @param y (`character`) vector of code.
#' @return `character` containing both `x` and `y` combined with
#'  unique names.
#' @keywords internal
.keep_code_name_unique <- function(x, y = character(0)) {
  combined <- c(x, y)
  if (length(names(combined)) == 0) {
    return(combined)
  }
  stats::setNames(combined, make.unique(names(combined)))
}
