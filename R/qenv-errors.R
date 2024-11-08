# needed to handle try-error
setOldClass("qenv.error")

#' @export
as.list.qenv.error <- function(x, ...) {
  stop(errorCondition(
    list(message = conditionMessage(x)),
    class = c("validation", "try-error", "simpleError")
  ))
}
