setOldClass("try-error")

setClass(
  "QuosureError",
  contains = "try-error",
  slots = c(message = "character", code = "character", evaluated_code = "character"),
  prototype = list(message = character(0), code = character(0), evaluated_code = character(0))
)

create_shiny_error <- function(object) {
  cond <- structure(
    list(message = paste("Error evaluating code:", object@message)),
    class = c("shiny.silent.error", "validation", "error", "condition")
  )
  stop(cond)
}
