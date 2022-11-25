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

  open_bracket_then_spaces <- "^\\{[[:blank:]]*"
  close_bracket_then_spaces <- "^\\}[[:blank:]]*"
  blank_line <- "^[[:blank:]]*$"
  four_spaces_at_start_of_line <- "^[[:blank:]]{4}"

  split_text <- unlist(strsplit(x, "\n", fixed = TRUE))

  # if text begins with "{   \n" and ends with "\n}   "
  if (grepl(open_bracket_then_spaces, utils::head(split_text, 1)) &&
      grepl(close_bracket_then_spaces, utils::tail(split_text, 1))) {
    # remove the first and last line
    split_text <- split_text[-c(1, length(split_text))]

    # if any line is not blank then indent
    if (!all(grepl(blank_line, split_text))) {
      return(gsub(four_spaces_at_start_of_line, "", split_text))
    }
  } else {
    return(split_text)
  }
}


# converts vector of expressions to character
format_expression <- function(code) {
  unlist(lapply(as.character(code), remove_enclosing_curly_braces))
}
