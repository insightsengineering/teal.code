#' Removes leading and trailing curly brackets from character
#' string and removes indentation of remaining contents
#'
#' @description `r lifecycle::badge("stable")`
#' @param x (`character`)\cr
#'
#' @return character string without curly braces
#' @keywords internal
remove_enclosing_curly_braces <- function(x) {
  checkmate::assert_character(x)
  if (length(x) == 0) {
    return(x)
  }

  open_bracket_and_spaces <- "^[[:blank:]]*\\{[[:blank:]]*$"
  close_bracket_and_spaces <- "^[[:blank:]]*\\}[[:blank:]]*$"
  blank_line <- "^[[:blank:]]*$"
  four_spaces_at_start_of_line <- "^[[:blank:]]{4}"

  split_text <- unlist(strsplit(x, "\n", fixed = TRUE))

  # if text begins with "{   \n" and ends with "\n}   "
  if (grepl(open_bracket_and_spaces, utils::head(split_text, 1)) &&
    grepl(close_bracket_and_spaces, utils::tail(split_text, 1))) {
    # remove the first and last line
    split_text <- split_text[-c(1, length(split_text))]

    # if any line is not blank then indent
    if (!all(grepl(blank_line, split_text))) {
      return(gsub(four_spaces_at_start_of_line, "", split_text))
    } else {
      return(split_text)
    }
  } else {
    return(split_text)
  }
}

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

# converts vector of expressions to character
format_expression <- function(code) {
  unlist(lapply(as.character(code), function(x) paste(remove_enclosing_curly_braces(x), collapse = "\n")))
}
