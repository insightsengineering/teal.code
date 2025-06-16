#' Evaluate code in `qenv`
#'
#' @details
#'
#' `eval_code()` evaluates given code in the `qenv` environment and appends it to the `code` slot.
#' Thus, if the `qenv` had been instantiated empty, contents of the environment are always a result of the stored code.
#'
#' @param object (`qenv`)
#' @param code (`character`, `language` or `expression`) code to evaluate.
#' It is possible to preserve original formatting of the `code` by providing a `character` or an
#' `expression` being a result of `parse(keep.source = TRUE)`.
#' @param ... ([`dots`]) additional arguments passed to future methods.
#'
#' @return
#' `qenv` environment with `code/expr` evaluated or `qenv.error` if evaluation fails.
#' The environment contains an attribute called `".Last.value"` which is the last evaluated value,
#' similarly to [base::.Last.value].
#'
#' @examples
#' # evaluate code in qenv
#' q <- qenv()
#' q <- eval_code(q, "a <- 1")
#' q <- eval_code(q, "b <- 2L # with comment")
#' q <- eval_code(q, quote(library(checkmate)))
#' q <- eval_code(q, expression(assert_number(a)))
#'
#' @aliases eval_code,qenv-method
#' @aliases eval_code,qenv.error-method
#' @seealso [within.qenv]
#' @export
setGeneric("eval_code", function(object, code, ...) standardGeneric("eval_code"))

setMethod("eval_code", signature = c(object = "qenv"), function(object, code, ...) {
  if (!is.language(code) && !is.character(code)) {
    stop("eval_code accepts code being language or character")
  }
  code <- .preprocess_code(code)
  # preprocess code to ensure it is a character vector
  .eval_code(object = object, code = code, ...)
})

setMethod("eval_code", signature = c(object = "qenv.error"), function(object, code, ...) object)

#' @keywords internal
.eval_code <- function(object, code, ...) {
  if (identical(trimws(code), "") || length(code) == 0) {
    return(object)
  }
  parsed_code <- parse(text = code, keep.source = TRUE)
  object@.xData <- rlang::env_clone(object@.xData, parent = parent.env(.GlobalEnv))
  if (length(parsed_code) == 0) {
    # empty code, or just comments
    attr(code, "dependency") <- extract_dependency(parsed_code) # in case comment contains @linksto tag
    object@code <- c(object@code, stats::setNames(list(code), sample.int(.Machine$integer.max, size = 1)))
    return(object)
  }
  code_split <- split_code(paste(code, collapse = "\n"))
  for (i in seq_along(code_split)) {
    current_code <- code_split[[i]]
    current_call <- parse(text = current_code, keep.source = TRUE)
    # Using withCallingHandlers to capture warnings and messages.
    # Using tryCatch to capture the error and abort further evaluation.
    x <- withCallingHandlers(
      tryCatch(
        {
          # needed to make sure that @.xData inherits from .GlobalEnv
          # could be changed when any new package is added to search path (through library or require call)
          qenv_last_value <- eval(current_call, envir = object@.xData)
          new_parent <- new.env(parent = parent.env(.GlobalEnv))
          new_parent[[".Last.value"]] <- qenv_last_value
          parent.env(object@.xData) <- new_parent
          NULL
        },
        error = function(e) {
          errorCondition(
            message = sprintf(
              "%s \n when evaluating qenv code:\n%s",
              cli::ansi_strip(conditionMessage(e)),
              current_code
            ),
            class = c("qenv.error", "try-error", "simpleError"),
            trace = unlist(c(object@code, list(current_code)))
          )
        }
      ),
      warning = function(w) {
        attr(current_code, "warning") <<- cli::ansi_strip(sprintf("> %s\n", conditionMessage(w)))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        attr(current_code, "message") <<- cli::ansi_strip(sprintf("> %s", conditionMessage(m)))
        invokeRestart("muffleMessage")
      }
    )

    if (!is.null(x)) {
      return(x)
    }
    attr(current_code, "dependency") <- extract_dependency(current_call)
    object@code <- c(object@code, stats::setNames(list(current_code), sample.int(.Machine$integer.max, size = 1)))
  }

  lockEnvironment(object@.xData, bindings = TRUE)
  object
}

setGeneric(".preprocess_code", function(code) standardGeneric(".preprocess_code"))
setMethod(".preprocess_code", signature = c("character"), function(code) paste(code, collapse = "\n"))
setMethod(".preprocess_code", signature = c("ANY"), function(code) {
  if (is.expression(code) && length(attr(code, "wholeSrcref"))) {
    paste(attr(code, "wholeSrcref"), collapse = "\n")
  } else {
    paste(
      vapply(lang2calls(code), deparse1, collapse = "\n", character(1L)),
      collapse = "\n"
    )
  }
})
