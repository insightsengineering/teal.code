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
  code <- paste(split_code(code), collapse = "\n")

  object@.xData <- rlang::env_clone(object@.xData, parent = parent.env(object@.xData))
  parsed_code <- parse(text = code, keep.source = TRUE)

  old <- evaluate::inject_funs(
    library = function(...) {
      x <- library(...)
      if (!identical(parent.env(object@.xData), parent.env(.GlobalEnv))) {
        parent.env(object@.xData) <- parent.env(.GlobalEnv)
      }
      invisible(x)
    }
  )
  out <- evaluate::evaluate(
    code,
    envir = object@.xData,
    stop_on_error = 1,
    output_handler = evaluate::new_output_handler(value = identity)
  )
  out <- evaluate::trim_intermediate_plots(out)

  evaluate::inject_funs(old) # remove library() override

  new_code <- list()
  for (this in out) {
    if (inherits(this, "source")) {
      this_code <- gsub("\n$", "", this$src)
      attr(this_code, "dependency") <- extract_dependency(parse(text = this_code, keep.source = TRUE))
      new_code <- c(new_code, stats::setNames(list(this_code), sample.int(.Machine$integer.max, size = 1)))
    } else {
      last_code <- new_code[[length(new_code)]]
      if (inherits(this, "error")) {
        return(
          errorCondition(
            message = sprintf(
              "%s \n when evaluating qenv code:\n%s",
              cli::ansi_strip(conditionMessage(this)),
              last_code
            ),
            class = c("qenv.error", "try-error", "simpleError"),
            trace = unlist(c(object@code, list(new_code)))
          )
        )
      }
      attr(last_code, "outputs") <- c(attr(last_code, "outputs"), list(this))
      new_code[[length(new_code)]] <- last_code
    }
  }

  object@code <- c(object@code, new_code)
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
