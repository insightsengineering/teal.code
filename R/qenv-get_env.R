#' Access environment included in `qenv`
#'
#' The access of environment included in `qenv@env` allows to e.g. list object names included in `qenv@env` slot.
#'
#' @param object (`qenv`)
#'
#' @return An `environment` stored in `qenv@env` slot.
#'
#' @examples
#' q <- qenv()
#' q1 <- within(q, {
#'   a <- 5
#'   b <- data.frame(x = 1:10)
#' })
#' get_env(q1)
#' ls(get_env(q1))
#'
#' @aliases get_env,qenv-method
#' @aliases get_env,qenv.error-method
#'
#' @export
setGeneric("get_env", function(object) {
  standardGeneric("get_env")
})

setMethod("get_env", "qenv", function(object) {
  object@env
})

setMethod("get_env", "qenv.error", function(object) {
  object
})
