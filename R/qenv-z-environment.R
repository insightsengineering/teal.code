#' @name qenv-inheritted
#' @rdname qenv
#'
#' @param x (`qenv`) object.
#' @param name (`character`) name of object.
#' @param pos,envir,all.names,pattern,sorted see [ls()] function for details.
#'
#' @usage x[[name]]
#' x$name
#' names(x)
#' ls(name, pos = -1L, envir = as.environment(pos),
#'    all.names = FALSE, pattern, sorted = TRUE)
#'
#' @details
#' `x[[name]]`, `x$name` and `get(name, x)` are generic \R operators to access the objects in the environment.
#' See [`[[`] for more details.
#' `names(x)` and `ls(x)` calls on the `qenv` object and will list all objects in the environment.
#'
#' @return `[[`, `$` and `get` return the value of the object named `name` in the `qenv` object.
#' @return `names` return a character vector of all the names of the objects in the `qenv` object.
#' @return `ls` return a character vector of the names of the objects in the `qenv` object.
#' It will only show the objects that are not named with a dot prefix, unless
#' the `all.names = TRUE`, which will show all objects.
NULL
