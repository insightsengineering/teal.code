setOldClass("try-error")

setClass(
  "QuosureError",
  contains = "try-error", # needed so shiny::isTruthy is false for shiny::validate
  slots = c(message = "character", code = "character", evaluated_code = "character"),
  prototype = list(message = character(0), code = character(0), evaluated_code = character(0))
)

create_shiny_error <- function(
  object,
  message = paste0("Error: '", object@message, "' when evaluating: ", paste(object@code, collapse = "\n"))
) {
  cond <- structure(
    list(message = message),
    class = c("shiny.silent.error", "validation", "error", "condition")
  )
  stop(cond)
}
