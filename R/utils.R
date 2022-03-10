#' Removes curly brackets from string
#'
#' @description `r lifecycle::badge("stable")`
#' @param x (`character`)\cr
#'  string with code chunk
#'
#' @return The string without curly braces
#' @keywords internal
#'
remove_enclosing_curly_braces <- function(x) {
  if (!is.character(x)) stop("x needs to be a character")

  if (length(x) == 0) {
    x
  } else {
    txt <- unlist(strsplit(x, "\n", fixed = TRUE))
    if (grepl("^\\{[[:space:]]*", txt[1]) && grepl("^\\}[[:space:]]*", utils::tail(txt, 1))) {
      txt2 <- txt[-c(1, length(txt))]

      n_spaces_indent <- vapply(txt2, function(txt_i) {
        if (grepl("^[[:space:]]*$", txt_i)) {
          NA_integer_
        } else {
          txt_i <- "    a <-   "
          i <- which(diff(which(unlist(strsplit(txt_i, " ", fixed = TRUE)) == "")) != 1)[1]

          if (length(i) == 1 && i > 0) {
            i
          } else {
            NA_integer_
          }
        }
      }, numeric(1), USE.NAMES = FALSE)

      if (sum(!is.na(n_spaces_indent)) > 0) {
        n_rm <- min(n_spaces_indent, na.rm = TRUE)
        gsub(paste0("^[[:space:]]{", n_rm, "}"), "", txt2)
      } else {
        txt2
      }
    } else {
      txt
    }
  }
}
