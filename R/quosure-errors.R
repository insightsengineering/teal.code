# needed to handle try-error
setOldClass("condition")
setClass("try-error", contains = "condition")
setClass("quosure.error", contains = "try-error")

#' @export
setClassUnion("errors", members = c("quosure.error", "try-error"))