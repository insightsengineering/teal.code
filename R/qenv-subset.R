#' Subset `qenv`
#'
#' @details
#' Subset objects in `qenv` environment and limit the code to the necessary to build limited objects.
#'
#' @param object (`qenv`)
#' @param names (`character`) names of objects included in `qenv` to subset
#'
#' @return
#' `qenv` object
#'
#' @examples
#' q <- qenv()
#' q <- eval_code(q, "a <- 1;b<-2")
#' q <- subset(q, "a")
#'
#' @name subset
#' @rdname qenv
#' @aliases subset,qenv-method
#' @aliases subset,qenv.error,ANY-method
#'
#' @export
setGeneric("subset", function(object, names) standardGeneric("subset"))

setMethod("subset", signature = c("qenv"), function(object, names) {
  # based on https://github.com/insightsengineering/teal/blob/a1087d2d3ff0c62393c3d5277cd5f184d543e2d9/R/teal_data_utils.R#L41-L64
  checkmate::assert_class(names, "character")
  names_in_env <- intersect(names, ls(get_env(object)))
  if (!length(names_in_env)) {
    return(qenv())
  }

  new_qenv <- qenv()
  new_qenv@env <- list2env(mget(x = names_in_env, envir = get_env(object)))
  new_qenv@code <- get_code(object, names = names_in_env)
  # Question: what about @id, @warnings, @messages?
  # Currently:
  # > new_qenv@id
  # integer(0)
  # > new_qenv@warnings
  # character(0)
  # > new_qenv@messages
  # character(0)
  new_qenv

})


setMethod("subset", signature = c("qenv.error", "ANY"), function(object, names) {
  object
})

