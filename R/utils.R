
#' Suppresses plot display in the IDE by opening a PDF graphics device
#'
#' This function opens a PDF graphics device using \code{\link[grDevices]{pdf}} to suppress
#' the plot display in the IDE. The purpose of this function is to avoid opening graphic devices
#' directly in the IDE.
#'
#' @param x lazy binding which generates the plot(s)
#'
#' @details The function uses \code{\link[base]{on.exit}} to ensure that the PDF graphics
#'          device is closed (using \code{\link[grDevices]{dev.off}}) when the function exits,
#'          regardless of whether it exits normally or due to an error. This is necessary to
#'          clean up the graphics device properly and avoid any potential issues.
#'
#' @import grDevices
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

format_expression <- function(code) {
  code <- lang2calls(code)
  paste(code, collapse = "\n")
}


#' recursively convert language object to list of simple calls
#' @param x a call or a list of calls
#' @keywords internal
lang2calls <- function(x) {
  if (is.call(x)) {
    if (identical(as.list(x)[[1L]], as.symbol("{"))) {
      as.list(x)[-1L]
    } else {
      list(x)
    }
  } else {
    unlist(lapply(x, lang2calls))
  }
}
