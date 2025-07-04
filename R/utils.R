#' Suppresses plot display in the IDE by opening a PDF graphics device
#'
#' This function opens a PDF graphics device using [`grDevices::pdf`] to suppress
#' the plot display in the IDE. The purpose of this function is to avoid opening graphic devices
#' directly in the IDE.
#'
#' @param x lazy binding which generates the plot(s)
#'
#' @details The function uses [`base::on.exit`] to ensure that the PDF graphics
#'          device is closed (using [`grDevices::dev.off`]) when the function exits,
#'          regardless of whether it exits normally or due to an error. This is necessary to
#'          clean up the graphics device properly and avoid any potential issues.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' dev_suppress(plot(1:10))
#' @export
dev_suppress <- function(x) {
  grDevices::pdf(nullfile())
  on.exit(grDevices::dev.off())
  force(x)
}

#' Separate calls
#'
#' Converts language object or lists of language objects to list of simple calls.
#'
#' @param x `language` object or a list of thereof
#' @return
#' Given a `call`, an `expression`, a list of `call`s or a list of `expression`s, returns a list of `calls`.
#' Symbols and atomic vectors (which may get mixed up in a list) are returned wrapped in list.
#' @examples
#' # use non-exported function from teal.code
#' lang2calls <- getFromNamespace("lang2calls", "teal.code")
#' expr <- expression(
#'   i <- iris,
#'   m <- mtcars
#' )
#' lang2calls(expr)
#' @keywords internal
lang2calls <- function(x) {
  if (is.atomic(x) || is.symbol(x)) {
    return(list(x))
  }
  if (is.call(x)) {
    if (identical(as.list(x)[[1L]], as.symbol("{"))) {
      as.list(x)[-1L]
    } else {
      list(x)
    }
  } else {
    unlist(lapply(x, lang2calls), recursive = FALSE)
  }
}

#' Obtain warnings or messages from code slot
#'
#' @param object (`qenv`)
#' @param what (`warning` or `message`)
#' @return `character(1)` containing combined message or `NULL` when no warnings/messages
#' @keywords internal
get_warn_message_util <- function(object, what) {
  checkmate::matchArg(what, choices = c("warning", "message"))
  messages <- lapply(
    object@code,
    function(x) {
      unlist(lapply(
        attr(x, "outputs"),
        function(el) {
          if (inherits(el, what)) {
            sprintf("> %s", conditionMessage(el))
          }
        }
      ))
    }
  )

  idx_warn <- which(sapply(messages, function(x) !is.null(x) && !identical(x, "")))
  if (!any(idx_warn)) {
    return(NULL)
  }
  messages <- messages[idx_warn]
  code <- object@code[idx_warn]

  lines <- mapply(
    warn = messages,
    expr = code,
    function(warn, expr) {
      sprintf("%s\nwhen running code:\n%s", trimws(warn), trimws(expr))
    }
  )

  sprintf(
    "~~~ %ss ~~~\n\n%s\n\n~~~ Trace ~~~\n\n%s",
    tools::toTitleCase(what),
    paste(lines, collapse = "\n\n"),
    paste(get_code(object), collapse = "\n")
  )
}
