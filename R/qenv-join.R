#' Join `qenv` objects
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Instead of [join()] use [c()].
#'
#' @param ... function is deprecated.
#'
#' @name join
#' @rdname join
#'
#' @export
join <- function(...) lifecycle::deprecate_stop("0.7.0", "join()", "c()")
