
#' @rdname quenv
#' @export
quenv <- function() {
  ans <- new.env()
  attr(ans, "code") <- list()
  attr(ans, "errors") <- list()
  attr(ans, "warnings") <- list()
  attr(ans, "messages") <- list()
  class(ans) <- c("quenv", class(ans))
  ans
}


#' @rdname quenv
#' @export
with.quenv <- function(data, expr, code, ...) {
  calls <- .prepare_code(if (!missing(expr)) substitute(expr), if (!missing(code)) code)
  extras <- list(...)
  lapply(calls, .eval_one, envir = data, enclos = parent.frame(), extras = extras)
  invisible(NULL)
}


#' @rdname quenv
#' @export
within.quenv <- function(data, expr, code, ...) {
  # Force a return even if some evaluation fails.
  on.exit(return(data))
  data <- .clone_quenv(data)
  calls <- .prepare_code(if (!missing(expr)) substitute(expr), if (!missing(code)) code)
  extras <- list(...)
  lapply(calls, .eval_one, envir = data, enclos = parent.frame(), extras = extras)
}



#' @keywords internal
#  prepare expression(s) for evaluation
.prepare_code <- function(expr, code) {
  # This function cannot handle missing arguments, so the caller passes if statements that return NULL if FALSE.

  # Get parent call to use in error messages.
  parent_call <- deparse1(match.call(definition = sys.function(2), call = sys.call(2)))

  if ((is.null(expr) && is.null(code)) || (!is.null(expr) && !is.null(code))) {
    stop("specify either \"expr\" or \"code\": ", parent_call, call. = FALSE)
  }

  if (!is.null(expr) && is.character(expr)) {
    stop(
      "character vector passed to \"expr\": ", parent_call, "\n  use the \"code\" argument instead",
      call. = FALSE
    )
  }

  calls <-
    if (is.null(code)) {
      # Add braces to expressions. Necessary for proper storage of some expressions (e.g. rm(x)).
      if (!grepl("^\\{", deparse1(expr))) {
        expr <- call("{", expr)
      }
      # Drop strings from compound expressions.
      Filter(Negate(is.character), as.list(expr)[-1])
    } else if (is.null(expr)) {
      if (is.character(code)) {
        # Character strings are converted to expression and unpacked into list.
        expr <- as.list(str2expression(code))
        unpack <- function(x) {
          if (identical(x[[1L]], as.symbol("{"))) {
            as.character(x[-1L])
          } else {
            deparse1(x)
          }
        }
        unlist(lapply(expr, unpack))
      } else if (is.language(code)) {
        # Language objects are wrapped in list.
        list(code)
      } else if (is.list(code) && all(vapply(code, is.call, logical(1L)))) {
        # Lists of language objects are passed on as is.
        code
      } else {
        stop("don't know how to handle this type of \"code\"", call. = FALSE)
      }
    }
  calls
}


#' @keywords internal
# evaluate one expression, log any conditions raised
.eval_one <- function(expression, envir, enclos, extras) {
  # Add empty string if no condition raised during evaluation.
  on.exit(
    lapply(c("errors", "warnings", "messages"), function(c) {
      if (length(attr(envir, c)) < length(attr(envir, "code"))) {
        attr(envir, c) <- append(attr(envir, c), "")
      }
    })
  )

  if (is.character(expression)) {
    expression <- str2lang(expression)
  }
  expression <- do.call(substitute, list(expr = expression, env = extras))

  attr(envir, "code") <- append(attr(envir, "code"), expression)
  tryCatch(
    eval(expression, envir = envir, enclos = enclos),
    message = function(m) attr(envir, "messages") <- append(attr(envir, "messages"), trimws(m$message)),
    warning = function(w) attr(envir, "warnings") <- append(attr(envir, "warnings"), w$message),
    error = function(e) {
      attr(envir, "errors") <- append(attr(envir, "errors"), e$message)
      stop(sprintf("Evaluation failed: %s", deparse1(expression)), call. = FALSE)
    }
  )
}

