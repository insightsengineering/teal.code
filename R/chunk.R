# Chunk Call (former chunk) Class ----

#' Code chunk - including expression and variables
#'
#' @name chunk_call
#' @description `r lifecycle::badge("deprecated")`
#' Chunks are being deprecated `qenv` objects should be used instead
#'
#' @keywords data
#'
#' @format An \code{\link{R6Class}} generator object
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(expression = NULL)}}{
#'     Create the object by setting the expression field which needs to
#'     be of type \code{call}.
#'   }
#'   \item{\code{eval(envir = parent.frame()}}{
#'     Evaluation of this code chunk. The value of the evaluated chunk with
#'     substitution of the variables by the environment values.
#'   }
#'   \item{\code{get_eval_info()}}{
#'     Returns a list with 5 elements: code, evaluation information, messages
#'     information, warnings information and errors information.
#'   }
#'   \item{\code{get_rcode(envir = parent.frame()}}{
#'     For the code chunk with the string of the R-Code is returned.
#'   }
#'   \item{\code{is_ok()}}{
#'     Get the information if the execution went right.
#'   }
#'   \item{\code{info()}}{
#'     Derive all slots this class carries. Slots are private and can just be derived
#'     by the \code{info} function which will return a list.
#'   }
#'   \item{\code{info_msg()}}{
#'     Get a formatted string of the evaluated code and caught warnings and errors.
#'   }
#'   \item{\code{get_errors()}}{
#'     Get a string of errors caught during evaluation.
#'   }
#'   \item{\code{get_warnings()}}{
#'     Get a string of warnings caught during evaluation.
#'   }
#'   \item{\code{get_messages()}}{
#'     Get a string of messages caught during evaluation.
#'   }
#'   \item{\code{is_evaluated()}}{
#'     Return `TRUE` if chunk was evaluated, `FALSE` otherwise.
#'   }
#'   \item{\code{is_warnings()}}{
#'     Return `TRUE` if chunk was evaluated and output any messages, `FALSE` otherwise.
#'   }
#'   \item{\code{is_messages()}}{
#'     Return `TRUE` if chunk was evaluated and output any messages, `FALSE` otherwise.
#'   }
#' }
#'
#' @aliases chunk_call$new chunk
#'
#' @examples
#' y <- 0
#' x <- chunk$new(expression = quote(y <- 1))
#'
#' x$get_rcode()
#' x$is_ok()
#' x$info_msg()
#' x$eval()
#' x$is_ok()
#' cat(x$info_msg())
#' y == 1
#'
#' # error handling
#' x <- chunk$new(expression = call("stop", "test"))
#' x$eval()
#' x$is_ok()
#' x$info_msg()
#' x$info()
NULL

#' @export
chunk_call <- R6::R6Class(
  classname = "chunk_call",
  # * public ----
  public = list(
    initialize = function(expression = NULL) {
      private$set_expression(expression)
      self$reset()
    },
    eval = function(envir = parent.frame()) {
      private$.is_evaluated <- TRUE
      private$.is_errors <- FALSE
      private$.is_warnings <- FALSE
      private$.is_messages <- FALSE
      private$eval_msg <- character(0)
      withCallingHandlers(
        tryCatch(
          eval(private$expression, envir = envir),
          error = function(e) {
            private$.is_errors <- TRUE
            private$errors <- paste0(conditionMessage(e), collapse = "\n")
            private$eval_msg <- private$errors
            return(invisible(NULL))
          }
        ),
        warning = function(w) {
          private$.is_warnings <- TRUE
          private$warnings <- c(private$warnings, gsub("[\r\n]$", "", conditionMessage(w)))
          invokeRestart("muffleWarning")
        },
        message = function(m) {
          private$.is_messages <- TRUE
          private$messages <- c(private$messages, gsub("[\r\n]$", "", conditionMessage(m)))
          invokeRestart("muffleMessage")
        }
      )
    },
    get_eval_info = function() {
      list(
        code = self$get_rcode(),
        eval_info = list(
          flag = self$is_evaluated()
        ),
        message_info = list(
          flag = self$is_messages(),
          msg = self$get_messages()
        ),
        warning_info = list(
          flag = self$is_warnings(),
          msg = self$get_warnings()
        ),
        error_info = list(
          flag = self$is_errors(),
          msg = self$get_errors()
        )
      )
    },
    get_rcode = function(envir = parent.frame()) {
      if (inherits(private$expression, "{")) {
        paste0(vapply(private$expression[-1], private$deparse, character(1)), collapse = "\n")
      } else {
        private$deparse(private$expression)
      }
    },
    is_ok = function() {
      private$.is_evaluated && !private$.is_errors
    },
    is_errors = function() {
      private$.is_evaluated && private$.is_errors
    },
    is_warnings = function() {
      private$.is_evaluated && private$.is_warnings
    },
    is_messages = function() {
      private$.is_evaluated && private$.is_messages
    },
    is_evaluated = function() {
      private$.is_evaluated
    },
    info = function() {
      list(
        expression = private$expression,
        is_evaluated = private$.is_evaluated,
        is_error = private$.is_errors,
        is_warning = private$.is_warnings,
        is_message = private$.is_messages,
        eval_msg = private$eval_msg
      )
    },
    info_msg = function() {
      if (!private$.is_evaluated) {
        return("Chunk not evaluated yet.")
      }

      msg <- ""

      if (self$is_messages()) {
        msg <- paste0(
          msg,
          "The following message(s) were outputted:\n",
          self$get_messages(),
          "\n\n"
        )
      }

      if (self$is_warnings()) {
        msg <- paste0(
          msg,
          "The following warning(s) occurred:\n",
          self$get_warnings(),
          "\n\n"
        )
      }

      if (self$is_errors()) {
        msg <- paste0(
          msg,
          "The following errors(s) occurred:\n",
          self$get_errors(),
          "\n\n"
        )
      }

      if (!self$is_errors() && !self$is_warnings()) {
        msg <- paste0(msg, "Everything went well!\n\n")
      }

      paste0(
        msg,
        "when evaluating the following code:\n",
        paste(styler::style_text(self$get_rcode()), collapse = "\n"),
        "\n"
      )
    },
    get_errors = function() {
      private$errors
    },
    get_warnings = function() {
      private$warnings
    },
    get_messages = function() {
      private$messages
    },
    reset = function() {
      private$.is_evaluated <- FALSE
      private$.is_errors <- FALSE
      private$.is_warnings <- FALSE
      private$.is_messages <- FALSE
      private$eval_msg <- character(0)
      private$errors <- character(0)
      private$warnings <- character(0)
      private$messages <- character(0)
    }
  ),
  # * private ----
  private = list(
    expression = NULL,
    .is_evaluated = FALSE,
    eval_msg = character(0),
    .is_warnings = FALSE,
    warnings = character(0),
    .is_errors = FALSE,
    errors = character(0),
    .is_messages = FALSE,
    messages = character(0),
    set_expression = function(expression) {
      stopifnot(is.call(expression) || is.expression(expression) || is.name(expression))
      private$expression <- expression
      invisible(self)
    },
    deparse = function(x) {
      paste0(
        remove_enclosing_curly_braces(
          paste0(
            deparse(x, width.cutoff = 500),
            collapse = "\n"
          )
        ),
        collapse = "\n"
      )
    }
  )
)

#' Chunk call
#' @name chunk
#' @rdname chunk_call
NULL

#' @export
chunk <- chunk_call

# Chunk Comment Class ----

#' Code Chunk comment
#'
#' @description `r lifecycle::badge("deprecated")`
#' Chunks are being deprecated `qenv` objects should be used instead
#' @name chunk_comment
#' @docType class
#' @keywords data
#'
#' @field comment (\code{character}) comment_to_show
#'
#' @format An \code{\link{R6Class}} generator object
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(comment = NULL)}}{
#'     Create the object by setting the field \code{comment}
#'   }
#'   \item{\code{get_rcode(envir = parent.frame()}}{
#'     For the code chunk with the string of the R-Code is returned.
#'   }
#'   \item{\code{get_eval_info()}}{
#'     Returns a list of information about evaluation.
#'   }
#' }
#'
#' @aliases chunk_comment$new
#'
#' @examples
#' x <- chunk_comment$new(comment = "I want a comment in this line")
#'
#' x$eval()
#' x$get_rcode()
#' safe_comment <- x$get_rcode()
#'
#' x <- chunk_comment$new(comment = "# I want a comment in this line")
#' x$eval()
#' x$get_rcode()
#' stopifnot(x$get_rcode() == safe_comment)
NULL

#' @export
chunk_comment <- R6::R6Class(
  classname = "chunk_comment",
  public = list(
    initialize = function(comment = NULL) {
      checkmate::assert_string(comment, null.ok = TRUE)
      private$comment <- comment
    },
    get_rcode = function(envir = NULL) {
      if (identical(private$comment, "")) {
        " "
      } else {
        # Remove spaces or comments containing # to have all comments
        # styled in the same way. Multi line comments are allowed.
        paste0(
          paste0(
            "# ",
            gsub("\\#[[:space:]]{0,1}", "", strsplit(private$comment, "\\n")[[1]])
          ),
          collapse = "\n"
        )
      }
    },
    eval = function(envir = NULL) {
      NULL
    },
    get_eval_info = function() {
      list(
        code = self$get_rcode(),
        eval_info = list(
          flag = FALSE
        ),
        message_info = list(
          flag = FALSE,
          msg = NULL
        ),
        warning_info = list(
          flag = FALSE,
          msg = NULL
        ),
        error_info = list(
          flag = FALSE,
          msg = NULL
        )
      )
    },
    is_warnings = function() {
      FALSE
    },
    is_errors = function() {
      FALSE
    },
    is_messages = function() {
      FALSE
    }
  ),
  private = list(
    comment = ""
  )
)
