#' Access environment included in `qenv`
#'
#' The access of environment included in the `qenv` that contains all data objects.
#'
#' @param object (`qenv`).
#'
#' @return An `environment` stored in `qenv` with all data objects.
#'
#' @examples
#' q <- qenv()
#' q1 <- within(q, {
#'   a <- 5
#'   b <- data.frame(x = 1:10)
#' })
#' get_env(q1)
#'
#' @aliases get_env,qenv-method
#' @aliases get_env,qenv.error-method
#'
#' @export
setGeneric("get_env", function(object) {
  standardGeneric("get_env")
})

setMethod("get_env", "qenv", function(object) object@.xData)

setMethod("get_env", "qenv.error", function(object) object)
