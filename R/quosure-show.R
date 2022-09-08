#' @export
setMethod("show", "Quosure", function(object) {
  obs <- names(as.list(object@env))
  if (length(obs) > 0) {
    cat(paste("A quosure object containing:", paste(obs, collapse = ", ")))
  } else {
    cat("A quosure object containing no objects")
  }
})


