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
#' @aliases join,qenv-method
#' @aliases join,qenv.error-method
#'
#' @export
setGeneric("join", function(...) standardGeneric("join"))

setMethod("join", signature = c("qenv"), function(...) {
  lifecycle::deprecate_stop("0.6.0", "join()", "c()")
})

setMethod("join", signature = c("qenv.error"), function(...) {
  lifecycle::deprecate_stop("0.6.0", "join()", "c()")
})
