#' Evaluate the code in the `qenv` environment
#'
#' Given code is evaluated in the `qenv` environment and appended to the `code` slot. This means
#' that state of the environment is always a result of the stored code (if `qenv` was initialized)
#' with reproducible code.
#'
#' @name eval_code
#'
#' @param object (`qenv`)
#' @param code (`character` or `language`) code to evaluate. Also accepts and stores comments
#'
#' @examples
#' q1 <- new_qenv(env = list2env(list(a = 1)), code = quote(a <- 1))
#' q2 <- eval_code(q1, quote(library(checkmate)))
#' q3 <- eval_code(q2, quote(assert_number(a)))
#'
#' @export
setGeneric("eval_code", function(object, code) {
  standardGeneric("eval_code")
})

#' @rdname eval_code
#' @export
setMethod("eval_code", signature = c("qenv", "expression"), function(object, code) {
  id <- sample.int(.Machine$integer.max, size = length(code))

  object@id <- c(object@id, id)
  object@env <- rlang::env_clone(object@env, parent = parent.env(.GlobalEnv))
  object@code <- c(object@code, code)

  current_warnings <- ""
  current_messages <- ""

  for (code_line in code) {
    x <- withCallingHandlers(
      tryCatch(
        {
          eval(code_line, envir = object@env)
          NULL
        },
        error = function(e) {
          errorCondition(
            message = sprintf(
              "%s \n when evaluating qenv code:\n %s",
              .ansi_strip(conditionMessage(e)),
              paste(code, collapse = "\n ")
            ),
            class = c("qenv.error", "try-error", "simpleError"),
            trace = object@code
          )
        }
      ),
      warning = function(w) {
        current_warnings <<- paste0(current_warnings, .ansi_strip(conditionMessage(w)))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        current_messages <<- paste0(current_messages, .ansi_strip(conditionMessage(m)))
        invokeRestart("muffleMessage")
      }
    )
    if (!is.null(x)) {
      return(x)
    }

    object@warnings <- c(object@warnings, current_warnings)
    object@messages <- c(object@messages, current_messages)
  }
  lockEnvironment(object@env, bindings = TRUE)
  object
})

#' @rdname eval_code
#' @export
setMethod("eval_code", signature = c("qenv", "language"), function(object, code) {
  code_char <- as.expression(code)
  eval_code(object, code_char)
})

#' @rdname eval_code
#' @export
setMethod("eval_code", signature = c("qenv", "character"), function(object, code) {
  eval_code(object, code = parse(text = code, keep.source = FALSE))
})

#' @rdname eval_code
#' @export
setMethod("eval_code", signature = "qenv.error", function(object, code) {
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
