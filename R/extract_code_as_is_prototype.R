code <- "
  # initial comment line 1
  # initial comment line 2
  a <- 1 # A comment
  b1 <- 2; b2 <- 2;b3 = 3 # inline comment
  c <- 3 # C comment
  # inbetween comment
  d <- 4
  # finishing comment line 1
  # finishing comment line 2
"

parsed_code <- parse(text = code)
comments <- extract_comments(parsed_code)
pd <- utils::getParseData(parsed_code)

pd <- pd[pd$token != "';'", ]

get_line_ids <- function(pd) {
  if (pd$token[1] == "COMMENT") {
    first_comment <- 1:(which(pd$parent == 0)[1]-1)
    pd_first_comment <- pd[first_comment, ]
    pd <- pd[-first_comment, ]

    n <- nrow(pd_first_comment)
    first_comment_ids <- data.frame(
      lines = c(pd_first_comment[1, "line1"], pd_first_comment[n, "line2"]),
      cols = c(pd_first_comment[1, "col1"], pd_first_comment[n, "col2"])
    )
  } else {
    first_comment_ids <- NULL
  }

  if (pd$token[nrow(pd)] == "COMMENT") {
    last_comment <- which(pd$parent == 0 & pd$token == "COMMENT")
    pd_last_comment <- pd[last_comment, ]
    pd <- pd[-last_comment, ]

    n <- nrow(pd_last_comment)
    last_comment_ids <- data.frame(
      lines = c(pd_last_comment[1, "line1"], pd_last_comment[n, "line2"]),
      cols = c(pd_last_comment[1, "col1"], pd_last_comment[n, "col2"])
    )
  } else {
    last_comment_ids <- NULL
  }


  calls_start <- which(pd$parent == 0)
  calls_end   <- c(which(pd$parent == 0)[-1] - 1, nrow(pd))

  call_ids <- list()
  for(i in seq_along(calls_start)) {
    call <- pd[c(calls_start[i], calls_end[i]), ]
    call_ids[[i]] <-
      data.frame(
       lines = c(call[1, "line1"], call[2, "line2"]),
       cols = c(call[1, "col1"], call[2, "col2"])
      )
  }


  Filter(Negate(is.null), c(list(first_comment_ids), call_ids, list(last_comment_ids)))
}

split_code <- function(code, lines_ids) {

  code_split <- strsplit(code, split = "\n", fixed = TRUE)[[1]]
  code_split_calls <- list()

  for(i in seq_along(lines_ids)) {

    code_lines <- code_split[lines_ids[[i]]$lines[1]:lines_ids[[i]]$lines[2]]

    if (length(code_lines) == 1) {
      code_lines <- substr(code_lines, lines_ids[[i]]$cols[1], lines_ids[[i]]$cols[2])
    } else {
      code_lines[1] <- substr(code_lines[1], lines_ids[[i]]$cols[1], nchar(code_lines[1]))
      code_lines[length(code_lines)] <- substr(code_lines[length(code_lines)], 1, lines_ids[[i]]$cols[2])
    }


    code_split_calls[[i]] <- paste(code_lines, collapse = "\n")
  }
  code_split_calls
}

lines_ids <- get_line_ids(pd)

code_by_calls <- split_code(code, lines_ids)

code_by_calls


