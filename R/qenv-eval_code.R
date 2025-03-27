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
#' @param label (`character` or `NULL`) when provided, a name of the `code` that can be used to pull
#' specific code elements with `get_code()`.
#'
#' @return
#' `qenv` environment with `code/expr` evaluated or `qenv.error` if evaluation fails.
#'
#' @examples
#' # evaluate code in qenv
#' q <- qenv()
#' q <- eval_code(q, "a <- 1")
#' q <- eval_code(q, "b <- 2L # with comment")
#' q <- eval_code(q, quote(library(checkmate)))
#' q <- eval_code(q, expression(assert_number(a)))
#'
#' @aliases eval_code,qenv,character,character-method
#' @aliases eval_code,qenv,language,character-method
#' @aliases eval_code,qenv,expression,character-method
#' @aliases eval_code,qenv.error,ANY,character-method
#'
#' @export
setGeneric("eval_code", function(object, code, label) {
  if (missing(label)) label <- ""
  stopifnot("Label needs to have length 1." = length(label) == 1)
  standardGeneric("eval_code")
})

setMethod("eval_code", signature = c("qenv", "character", "character"), function(object, code, label = "") {
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
          eval(current_call, envir = object@.xData)
          if (!identical(parent.env(object@.xData), parent.env(.GlobalEnv))) {
            # needed to make sure that @.xData is always a sibling of .GlobalEnv
            # could be changed when any new package is added to search path (through library or require call)
            parent.env(object@.xData) <- parent.env(.GlobalEnv)
          }
          NULL
        },
        error = function(e) {
          errorCondition(
            message = sprintf(
              "%s \n when evaluating qenv code:\n%s",
              .ansi_strip(conditionMessage(e)),
              current_code
            ),
            class = c("qenv.error", "try-error", "simpleError"),
            trace = unlist(c(object@code, list(current_code)))
          )
        }
      ),
      warning = function(w) {
        attr(current_code, "warning") <<- .ansi_strip(sprintf("> %s\n", conditionMessage(w)))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        attr(current_code, "message") <<- .ansi_strip(sprintf("> %s", conditionMessage(m)))
        invokeRestart("muffleMessage")
      }
    )

    if (!is.null(x)) {
      return(x)
    }
    attr(current_code, "dependency") <- extract_dependency(current_call)
    attr(current_code, "label") <- label
    object@code <- c(object@code, stats::setNames(list(current_code), sample.int(.Machine$integer.max, size = 1)))
  }

  lockEnvironment(object@.xData, bindings = TRUE)
  object
})

setMethod("eval_code", signature = c("qenv", "language", "character"), function(object, code, label = "") {
  eval_code(object, code = paste(vapply(lang2calls(code), deparse1, collapse = "\n", character(1L)), collapse = "\n"), label)
})

setMethod("eval_code", signature = c("qenv", "expression", "character"), function(object, code, label = "") {
  srcref <- attr(code, "wholeSrcref")
  if (length(srcref)) {
    eval_code(object, code = paste(attr(code, "wholeSrcref"), collapse = "\n"), label = label)
  } else {
    Reduce(function(obj, expr) eval_code(obj, expr, label), x = code, init = object)
  }
})

setMethod("eval_code", signature = c("qenv.error", "ANY", "character"), function(object, code, label = "") {
  object
})

# if cli is installed rlang adds terminal printing characters
# which need to be removed
.ansi_strip <- function(chr) {
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::ansi_strip(chr)
  } else {
    chr
  }
}

get_code_attr <- function(qenv, attr) {
  unlist(lapply(qenv@code, function(x) attr(x, attr)))
}
