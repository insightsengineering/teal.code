#' Get code from `Quosure`
#'
#' @name get_code
#' @param object (`Quosure`)
#' @return named `character` with the reproducible code.
#' @export
setGeneric("get_code", function(object) {
  standardGeneric("get_code")
})

#' @rdname get_code
#' @export
setMethod("get_code", signature = "Quosure", function(object) {
  object@code
})
