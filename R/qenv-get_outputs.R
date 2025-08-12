#' Get outputs
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' `eval_code` evaluates code silently so plots and prints don't show up in the console or graphic devices.
#' If one wants to use an output outside of the `qenv` (e.g. use a graph in `renderPlot`) then use `get_outputs`.
#' @param object (`qenv`)
#' @return list of outputs generated in a `qenv``
#' @examples
#' q <- eval_code(
#'   qenv(),
#'   quote({
#'     a <- 1
#'     print("I'm an output")
#'     plot(1)
#'   })
#' )
#' get_outputs(q)
#'
#' @aliases get_outputs,qenv-method
#'
#' @export
setGeneric("get_outputs", function(object) standardGeneric("get_outputs"))

setMethod("get_outputs", signature = "qenv", function(object) {
  Reduce(
    function(x, y) c(x, attr(y, "outputs")),
    init = list(),
    x = object@code
  )
})
