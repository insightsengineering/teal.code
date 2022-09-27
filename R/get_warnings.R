#' Get the warnings of qenv object
#'
#' @param object (`qenv`)
#'
#' @return character vector of warnings
#' @export
#'
#' @examples
#' data_q <- new_qenv() %>% eval_code("iris_data <- iris")
#' warning_qenv <- eval_code(
#'   data_q,
#'   bquote(p <- hist(iris_data[, .("Sepal.Length")], ff = ""))
#' )
#' get_warnings(warning_qenv)
#'
get_warnings <- function(object) {
  attr(object@code, "warning")
}

#' Get the messages of qenv object
#'
#' @param object (`qenv`)
#'
#' @return character vector of messages
#' @export
#'
#' @examples
#' data_q <- new_qenv() %>% eval_code(bquote(message("This is a message")))
#' get_messages(data_q)
#'
get_messages <- function(object) {
  attr(object@code, "message")
}
