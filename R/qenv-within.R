

#' @export
within.qenv <- function(data, expr, code, ...) {
  extras <- list(...)
  calls <- .prepare_code(if (!missing(expr)) substitute(expr), if (!missing(code)) code)
  code_added <- attr(calls, "code")

  id <- sample.int(.Machine$integer.max, size = length(calls))
  data@id <- c(data@id, id)
  data@env <- rlang::env_clone(data@env, parent = parent.env(.GlobalEnv))
  # TODO change when @code can hold character
  data@code <- c(data@code, code_added)
  # data@code <- paste(c(data@code, attr(calls, "code")), collapse = "\n") # nolint
  # END TODO

  current_warnings <- ""
  current_messages <- ""

  for (call in calls) {
    call <- do.call(substitute, list(expr = call, env = extras))
    # Using withCallingHandlers to capture ALL warnings and messages.
    # Using tryCatch to capture the FIRST error and abort further evaluation.
    x <- withCallingHandlers(
      tryCatch(
        {
          eval(call, envir = data@env, enclos = parent.frame())
          NULL
        },
        error = function(e) {
          errorCondition(
            message = sprintf(
              "%s \n when evaluating qenv code:\n%s",
              .ansi_strip(conditionMessage(e)),
              paste(format_expression(code_added), collapse = "\n")
            ),
            class = c("qenv.error", "try-error", "simpleError"),
            trace = data@code
          )
        }
      ),
      warning = function(w) {
        current_warnings <<- paste0(current_warnings, .ansi_strip(sprintf("> %s\n", conditionMessage(w))))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        current_messages <<- paste0(current_messages, .ansi_strip(sprintf("> %s", conditionMessage(m))))
        invokeRestart("muffleMessage")
      }
    )
    if (!is.null(x)) {
      return(x)
    }

    data@warnings <- c(data@warnings, current_warnings)
    data@messages <- c(data@messages, current_messages)
  }
  lockEnvironment(data@env, bindings = TRUE)
  data
}



#' @keywords internal
#' @noRd
# prepare expression(s) for evaluation
# returns a list of calls with attribute 'code' that contains the original text (if any) or the deparsed calls
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

  if (!is.null(expr) && is.symbol(expr)) {
    warning(
      "symbol passed to \"expr\": ", parent_call, "\n",
      "  evaluated within qenv as is", "\n",
      "  this argument is meant for literal expressions", "\n",
      "  variables containing code should be passed to \"code\" instead",
      call. = FALSE
    )
  }

  if (is.null(code)) .process_expr(expr) else if (is.null(expr)) .process_code(code)
}

#' @keywords internal
#' @noRd
# process `expr` argument to `calls` in .prepare_code
.process_expr <- function(expr) {
  # Add braces to expressions. Necessary for proper storage of some expressions (e.g. rm(x)).
  if (!grepl("^\\{", deparse1(expr))) {
    expr <- call("{", expr)
  }
  # Drop strings from compound expressions.
  calls <- Filter(Negate(is.character), as.list(expr)[-1])
  attr(calls, "code") <- paste(deparse(expr), collapse = "\n")
  calls
}

#' @keywords internal
#' @noRd
# process `code` argument to `calls` in .prepare_code
.process_code <- function(code) {
  if (is.character(code)) {
    # Character strings are converted to expression and then to a list of calls.
    unpack <- function(x) {
      if (identical(x[[1L]], as.symbol("{"))) {
        as.list(x[-1L])
      } else {
        x
      }
    }
    calls <- unlist(lapply(str2expression(code), unpack))
    attr(calls, "code") <- paste(code, collapse = "\n")
  } else if (is.language(code)) {
    calls <-
      if (as.list(code)[[1L]] == as.name("{")) {
        # Compound calls are disassembled into a list of single calls.
        calls <- as.list(code)[-1L]
      } else {
        # Language objects are wrapped in list.
        list(code)
      }
    attr(calls, "code") <- paste(deparse(code), collapse = "\n")
  } else if (is.list(code) && all(vapply(code, is.call, logical(1L)))) {
    # Lists of language objects are passed on as is.
    calls <- code
    attr(calls, "code") <- paste(code, collapse = "\n")
  } else {
    stop("don't know how to handle this type of \"code\"", call. = FALSE)
  }
  calls
}
