#' Evaluate code in `qenv`
#'
#' @details
#' `eval_code` evaluates given code in the `qenv` environment and appends it to the `code` slot.
#' Thus, if the `qenv` had been instantiated empty, contents of the environment are always a result of the stored code.
#'
#' @param object (`qenv`)
#' @param code (`character` or `language`) code to evaluate. If `character`, comments are retained.
#'
#' @return
#' `eval_code` returns a `qenv` object with `expr` evaluated or `qenv.error` if evaluation fails.
#'
#' @examples
#' # evaluate code in qenv
#' q <- qenv()
#' q <- eval_code(q, "a <- 1")
#' q <- eval_code(q, quote(library(checkmate)))
#' q <- eval_code(q, expression(assert_number(a)))
#'
#' @name eval_code
#' @rdname qenv
#' @aliases eval_code,qenv,character-method
#' @aliases eval_code,qenv,language-method
#' @aliases eval_code,qenv,expression-method
#' @aliases eval_code,qenv.error,ANY-method
#'
#' @export
setGeneric("eval_code", function(object, code) standardGeneric("eval_code"))

setMethod("eval_code", signature = c("qenv", "character"), function(object, code) {
  parsed_code <- parse(text = code, keep.source = TRUE)
  if (length(parsed_code) == 0) {
    return(object)
  }

  object@env <- rlang::env_clone(object@env, parent = parent.env(.GlobalEnv))
  code_split <- split_code(paste(code, collapse = "\n"))

  for (i in seq_along(code_split)) {
    current_code <- code_split[[i]]
    current_call <- parse(text = current_code, keep.source = FALSE)

    # Using withCallingHandlers to capture warnings and messages.
    # Using tryCatch to capture the error and abort further evaluation.
    x <- withCallingHandlers(
      tryCatch(
        {
          eval(current_call, envir = object@env)
          if (!identical(parent.env(object@env), parent.env(.GlobalEnv))) {
            # needed to make sure that @env is always a sibling of .GlobalEnv
            # could be changed when any new package is added to search path (through library or require call)
            parent.env(object@env) <- parent.env(.GlobalEnv)
          }
          NULL
        },
        error = function(e) {
          errorCondition(
            message = sprintf(
              "%s \n when evaluating qenv code:\n%s",
              .ansi_strip(conditionMessage(e)),
              deparse1(current_call)
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

    attr(current_code, "id") <- sample.int(.Machine$integer.max, size = 1)
    object@code <- c(object@code, list(current_code))
  }

  lockEnvironment(object@env, bindings = TRUE)
  object
})

setMethod("eval_code", signature = c("qenv", "language"), function(object, code) {
  eval_code(object, code = paste(lang2calls(code), collapse = "\n"))
})

setMethod("eval_code", signature = c("qenv", "expression"), function(object, code) {
  # todo: if has srcfile then get original text!
  eval_code(object, code = paste(lang2calls(code), collapse = "\n"))
})

setMethod("eval_code", signature = c("qenv.error", "ANY"), function(object, code) {
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
  # unlist(lapply(qenv@code, attr, attr)) # somehow doesn't work
  unlist(lapply(qenv@code, function(x) attr(x, attr)))
}
