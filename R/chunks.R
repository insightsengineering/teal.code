# Chunks Stack Class ----

#' Multiple Code chunk handler
#'
#' @description `r lifecycle::badge("stable")`
#' @name chunks
#' @docType class
#' @keywords data
#'
#' @description
#' \code{chunks} is a specialized stack for call objects and comments. It is intended to capture and evaluate R code for
#' a sequence of analysis steps.
#'
#' @format An \code{\link{R6Class}} generator object
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(envir = new.env())}}{
#'     Initializes the object.
#'
#'     \code{envir}: (optional) environment to get objects from to chunks environment
#'
#'   }
#'   \item{\code{push(x, id)}}{
#'       This method adds an expression code chunk or comment code chunk to this object.
#'       If no \code{id} is given a random id is generated.
#'
#'       The method needs the parameter \code{x}. In case this is of type \code{call} and the code will
#'       be evaluated via \code{eval}. Additionally, the code can be given as a string and a comment will be inserted.
#'
#'   }
#'   \item{\code{eval()}}{
#'        This method applies the \code{eval_id} method to each code chunk stored inside this object.
#'
#'        This method should not be evaluated multiple times, else it throws a warning. This is due to the
#'        fact that multiple evaluations of the following second code snippet is dangerous:
#'        \itemize{
#'          \item{1 }{a <- 2}
#'          \item{2 }{a <- a + 1}
#'        }
#'        \code{eval} evaluates code just once and warn user if one tries to evaluate multiple times.
#'   }
#'   \item{\code{get_rcode()}}{
#'        Run \code{get_rcode_id} for all code chunks inside this object.
#'   }
#'   \item{\code{get_warnings()}}{
#'        Return warnings encountered during execution of the code inside the object.
#'   }
#'   \item{\code{get_messages()}}{
#'        Return list of warnings encountered during execution of the code inside the object.
#'   }
#'   \item{\code{eval_info(ids = private$get_call_ids())}}{
#'        Accepts a vector of chunk ids.
#'        Returns a list with evaluation information about each chunk (messages, warnings, errors).
#'        Default is to hide information about all \code{chunk_comment} objects. Pass chunk ids
#'        to see information about specific ids.
#'   }
#'   \item{\code{reset()}}{
#'       Delete all chunks handles inside this stack and reset environment.
#'   }
#'   \item{\code{info()}}{
#'       Print all private content. Used for debugging only.
#'   }
#'   \item{\code{info_msg()}}{
#'       Get an information about how the evaluation of the chunk stack went.
#'         Most useful if \code{is_ok()} returns \code{FALSE}.
#'   }
#'   \item{\code{get(var, inherits = FALSE)}}{
#'       Returns variable from chunks environment. For more information see
#'       \link[base:get]{get}.
#'   }
#'   \item{\code{is_ok()}}{
#'       Checks if all chunks are evaluated and if there is any error.
#'   }
#'   \item{\code{get_eval_msg()}}{
#'       Returns error message if there is any error.
#'   }
#'   \item{\code{validate_is_ok(msg)}}{
#'       Return `shiny's` validation error with custom message if \code{is_ok} returns \code{FALSE}.
#'   }
#'   \item{\code{validate_is(var, class, msg)}}{
#'       Return `shiny's` validation error with custom message if \code{var} is not of class \code{class}.
#'   }
#'   \item{\code{validate_all(var, class, msg)}}{
#'       Return `shiny's` validation error with custom message if \code{is_ok} returns \code{FALSE} or
#'       \code{var} is not of class \code{class}.
#'   }
#'   \item{\code{ls(all.names = FALSE)}}{
#'       Return a vector of bindings present in the environment of a chunks object. See \link{ls}
#'       for more information.
#'   }
#'   \item{\code{get_reactive_summary()}}{
#'       Return a \code{shiny::reactiveValues} object with three elements: \code{msgs}, `warnings`, `errors`.
#'       Each element is a logical vector with values corresponding to `chunk` objects inside `chunks`.
#'       `TRUE` in the \code{msgs} vector indicates at least one message was returned in a corresponding chunk.
#'       `TRUE` in the `warnings` vector indicates at least one warning was returned.
#'       `TRUE` in the `errors` vector indicates an error was thrown.
#'   }
#' }
#'
#' @examples
#' x <- chunks$new()
#'
#' x$push(id = "test_1", x = call("print", 1))
#' res <- x$eval()
#' res
#'
#' x$push(id = "test_2", x = call("print", 2))
#' x$push(id = "test_3", x = call("print", 3))
#' res <- x$eval()
#' res
#' # Below gives a warning
#' \dontrun{
#' chunks_eval(chunks = x)
#' }
#'
#' x$push(id = "test_4", x = call("print", 4))
#' x$get_rcode()
#'
#' x$get_rcode()
#'
#' # Error handling
#' x$push(id = "test_error", x = call("stop", "test"))
#' x$eval() # returns NULL, does not rise an error
#' x$is_ok() # FALSE
#' x$info_msg()
#' # Below used inside shiny apps
#' \dontrun{
#' x$validate_is_ok("This is my custom error message")
#' }
#'
#' x$info()
#'
#' # Analysis Example
#'
#' x <- chunks$new()
#' x$push("# Prepare Data")
#' x$push(quote(x <- iris$Sepal.Length))
#' x$push(quote({
#'   y <- iris$Petal.Length
#'   {
#'     col <- iris$Species
#'   }
#' }))
#'
#' x$eval()
#' x$get("col")[1:5]
#' \dontrun{
#' x$get("mean") # should not scope in getter
#' }
#'
#' x$push("") # newline
#' x$push("# Analysis")
#' x$push(quote(plot(x, y, col = col)))
#'
#' x$is_ok()
#' x$eval()
#' x$is_ok() # now all chunks were evaluated and no errors occured
#'
#' cat(paste(x$get_rcode(), collapse = "\n"))
NULL

#' @export
chunks <- R6::R6Class(
  classname = "chunks",
  # * public ----
  public = list(
    initialize = function(envir = new.env()) {
      checkmate::assert_environment(envir, null.ok = TRUE)
      private$reset_env(envir = envir)
      private$reactive_summary <- reactiveValues(msgs = logical(0), warnings = logical(0), errors = logical(0))

      return(invisible(self))
    },
    reset = function(envir = parent.frame()) {
      private$reset_chunks()
      private$reset_env(envir = envir)
      # store a snapshot of the current environment for uneval
      private$reset_snapshot()

      return(invisible(NULL))
    },
    push = function(x, id = NULL) {
      stopifnot(is.call(x) || is.expression(x) || is.name(x) || checkmate::test_string(x))
      if (is.null(id)) {
        id <- private$get_new_id()
      }
      checkmate::assert_string(id)

      if (private$id_exists(id)) {
        warning("push() cannot be used to change chunks. Please use append with desired ID.")
        return(invisible(NULL))
      }

      private$id <- c(private$id, id)
      private$is_remaining <- c(private$is_remaining, !is.character(x)) # calls are remaining
      private$is_evaluated <- c(private$is_evaluated, is.character(x)) # comments and new line (NULL) are evaluated
      private$set_is_error(FALSE)
      private$set_is_warning(FALSE)
      private$set_is_message(FALSE)

      new_chunk <- if (is.character(x)) {
        chunk_comment$new(comment = x)
      } else {
        chunk_call$new(expression = x)
      }
      private$code_chunks <- append(private$code_chunks, new_chunk)

      return(invisible(NULL))
    },
    push_chunks = function(x, overwrite = FALSE) {
      x_private <- x$.__enclos_env__$private
      x_not_remaining <- any(!x_private$is_remaining)

      # in case this chunk is NOT evaluated and x is evaluated, uneval x
      if (private$any_remaining() && x_not_remaining) {
        x$uneval()
      }
      names_self <- names(private$envir)
      names_x <- names(x_private$envir)
      overwrite_vars <- intersect(names_x, names_self)

      same_fun <- function(var) {
        identical(x$get(var), self$get(var))
      }

      same_obj <- vapply(
        overwrite_vars, function(x) {
          same_fun(x)
        },
        logical(1)
      )

      overwrite_vars <- overwrite_vars[!same_obj]

      if (length(overwrite_vars) != 0 && !overwrite) {
        stop(paste(
          "chunks_push_chunks does not allow overwriting already calculated values.",
          " Following variables would have been overwritten:",
          paste("    -", overwrite_vars, collapse = "\n"),
          sep = "\n"
        ))
      } else {
        private$code_chunks <- append(private$code_chunks, x_private$code_chunks)
        id_suffix <- paste0(sample(LETTERS, 5, replace = TRUE), collapse = "")
        for (xid in x_private$id) {
          if (xid %in% private$id) {
            private$id <- c(
              private$id,
              paste0(xid, "_", id_suffix)
            )
          } else {
            private$id <- c(private$id, xid)
          }
        }
        private$is_remaining <- c(private$is_remaining, x_private$is_remaining)
        private$is_evaluated <- c(private$is_evaluated, x_private$is_evaluated)
        private$set_is_error(c(private$is_error, x_private$is_error), idx = NULL)
        private$set_is_warning(c(private$is_warning, x_private$is_warning), idx = NULL)
        private$set_is_message(c(private$is_message, x_private$is_message), idx = NULL)
        lapply(names_x, function(variable) {
          assign(variable, x$get(variable), envir = private$envir)
        })
      }

      return(invisible(NULL))
    },
    eval = function() {
      if (private$is_empty()) {
        warning("These chunks do not contain any code to evaluate, yet.")
        return(invisible(NULL))
      }
      if (!private$any_remaining()) {
        warning("All chunks were already evaluated The last evaluated chunk result is returned.")
        return(private$latest_result)
      }
      if (private$any_error()) {
        warning(self$info_msg())
        return(invisible(NULL))
      }

      for (i in private$id_remaining()) {
        private$eval_id(i)

        x <- private$get_chunk(i)
        if (!inherits(x, "chunk_comment")) {
          idx <- private$get_idx(i)
          if (!x$is_ok()) {
            private$eval_msg <- x$info_msg()

            return(invisible(NULL))
          }
        }
      }

      return(private$latest_result)
    },
    uneval = function(envir = parent.frame(), overwrite = FALSE) {
      if (overwrite) {
        private$reset_env(envir)
      } else {
        private$reset_env(private$envir_reset)
      }
      private$is_remaining <- rep(TRUE, length(private$is_remaining))

      private$set_is_error(rep(FALSE, length(private$is_error)), idx = NULL)
      private$set_is_warning(rep(FALSE, length(private$is_warning)), idx = NULL)
      private$set_is_message(rep(FALSE, length(private$is_message)), idx = NULL)
      private$is_evaluated <- rep(FALSE, length(private$is_evaluated), idx = NULL)
      return(invisible(NULL))
    },
    eval_info = function(ids = private$get_call_ids()) {
      stopifnot(all(vapply(ids, function(x) private$id_exists(x), logical(1))))
      lapply(
        ids,
        function(id) {
          c(
            id = id,
            private$get_chunk(id)$get_eval_info()
          )
        }
      )
    },
    get = function(var, inherits = FALSE) {
      checkmate::assert_string(var)
      if (!exists(var, private$envir, inherits = inherits)) {
        warning(paste0("\"", var, "\" does not exist in the chunks."))
        return(NULL)
      }

      return(get(var, envir = private$envir, inherits = inherits))
    },
    ls = function(all.names = FALSE) { # nolint
      return(ls(private$envir, all.names = all.names))
    },
    get_rcode = function(chunk_ids = character(0)) {
      checkmate::assert_character(chunk_ids)

      res <- if (private$is_empty()) {
        character(0)
      } else if (length(chunk_ids) > 0) {
        vapply(chunk_ids, private$get_rcode_id, character(1))
      } else {
        vapply(private$id, private$get_rcode_id, character(1))
      }

      return(res)
    },
    get_warnings = function(chunk_ids = private$id) {
      checkmate::assert_character(chunk_ids)

      warnings <- if (length(chunk_ids) > 0) {
        # sapply, because get_warnings_id() might return character(0) or character(1)
        sapply(chunk_ids, private$get_warnings_id, simplify = FALSE)
      } else {
        character(0)
      }

      return(warnings)
    },
    get_messages = function(chunk_ids = private$id) {
      checkmate::assert_character(chunk_ids)

      messages <- if (length(chunk_ids) > 0) {
        # sapply, because get_messages_id() might return character(0) or character(1)
        sapply(chunk_ids, private$get_messages_id, simplify = FALSE)
      } else {
        character(0)
      }

      return(messages)
    },
    get_reactive_summary = function() {
      private$reactive_summary
    },
    is_ok = function() {
      !isTRUE(private$any_remaining()) && !isTRUE(private$any_error())
    },
    validate_is_ok = function(msg = NULL) {
      checkmate::assert_string(msg, null.ok = TRUE)
      validate(need(self$is_ok(), if (is.null(msg)) private$eval_msg else msg))
    },
    validate_is = function(var, class, msg = NULL) {
      checkmate::assert_string(var)
      checkmate::assert_string(class)
      checkmate::assert_string(msg, null.ok = TRUE)

      validate(
        need(
          inherits(self$get(var), class),
          `if`(
            !is.null(msg),
            msg,
            paste0(
              "Variable '", var, "' is of class ", paste0(class(self$get(var)), collapse = " "),
              " instead of ", class, "."
            )
          )
        )
      )
    },
    validate_all = function(var, class, msg = NULL) {
      self$validate_is_ok(msg)
      self$validate_is(var, class, msg)
    },
    info = function() {
      res <- list(
        envir = private$envir,
        id = private$id,
        is_evaluated = private$is_evaluated,
        is_remaining = private$is_remaining,
        is_error = private$is_error,
        is_warning = private$is_warning,
        is_message = private$is_message,
        eval_msg = private$eval_msg,
        code_chunks = private$code_chunks,
        latest_result = private$latest_result
      )

      return(res)
    },
    info_msg = function() {
      if (!any(private$is_evaluated)) {
        return("Chunks not evaluated yet.\n")
      }

      if (any(private$is_error)) {
        msg <- private$eval_msg
      } else {
        msg <- "Tried to evaluate following code:\n"
        r_code <- paste0(self$get_rcode(), collapse = "\n")
        msg_log <- "\nEverything went well!\n"
        msg <- paste0(msg, r_code, msg_log)
      }
      msg
    },
    get_eval_msg = function() {
      if (!any(private$is_evaluated)) {
        return("Chunks not evaluated yet.\n")
      }
      private$eval_msg
    }
  ),
  # * private ----
  private = list(
    envir = new.env(),
    envir_reset = new.env(),
    id = character(0),
    is_evaluated = logical(0),
    is_remaining = logical(0),
    is_error = logical(0),
    is_warning = logical(0),
    is_message = logical(0),
    eval_msg = character(0),
    code_chunks = list(),
    latest_result = NULL,
    reactive_summary = NULL,
    get_latest_id = function() {
      utils::tail(private$id, 1)
    },
    get_new_id = function() {
      paste_id <- function(id) paste0("chunk_", id)
      new_num_id <- length(private$id) + 1
      new_id <- paste_id(new_num_id)
      while (new_id %in% private$id) {
        new_num_id <- new_num_id + 1
        new_id <- paste_id(new_num_id)
      }
      new_id
    },
    get_idx = function(id) {
      checkmate::assert_string(id)
      stopifnot(private$id_exists(id))
      which(id == private$id)
    },
    get_call_ids = function() {
      private$id[vapply(
        private$id,
        function(x) inherits(private$get_chunk(x), "chunk_call"),
        logical(1)
      )]
    },
    get_comment_ids = function() {
      private$id[vapply(
        private$id,
        function(x) inherits(private$get_chunk(x), "chunk_comment"),
        logical(1)
      )]
    },
    id_evaluated = function() {
      private$id[private$is_evaluated]
    },
    id_remaining = function() {
      private$id[private$is_remaining]
    },
    any_remaining = function() {
      any(private$is_remaining)
    },
    id_error = function() {
      private$id[private$is_error]
    },
    any_error = function() {
      any(private$is_error)
    },
    id_warning = function() {
      private$id[private$is_warning]
    },
    any_warning = function() {
      any(private$is_warning)
    },
    id_message = function() {
      private$id[private$is_message]
    },
    any_message = function() {
      any(private$is_message)
    },
    get_chunk = function(id) {
      checkmate::assert_string(id)
      stopifnot(private$id_exists(id))
      idx <- private$get_idx(id)
      private$code_chunks[[idx]]
    },
    reset_chunks = function() {
      private$id <- character(0)
      private$is_evaluated <- logical(0)
      private$is_remaining <- logical(0)
      private$set_is_error(logical(0), idx = NULL)
      private$set_is_warning(logical(0), idx = NULL)
      private$set_is_message(logical(0), idx = NULL)
      private$eval_msg <- character(0)
      private$code_chunks <- list()
      private$latest_result <- NULL
    },
    reset_env = function(envir = parent.frame()) {
      checkmate::assert_environment(envir)
      private$envir <- new.env()
      # isolate from .GlobalEnv
      parent.env(private$envir) <- parent.env(.GlobalEnv)
      clone_env(envir_from = envir, envir_to = private$envir)
    },
    # store the reset environment to allow a reset
    # after using "uneval"
    #
    # This is needed in case someone uses the uneval
    # function inside a different environment than
    # the first eval function
    #
    # imagine you assing "a" and "b" at "reset"
    # you want to get the "sum"
    # if you run "uneval" in a different parent.frame, you
    # still need the sum to be "a + b".
    # Therefore this snapshot needs to be stored, because
    # else you get "ERROR, 'a' not found"
    reset_snapshot = function() {
      private$envir_reset <- private$envir
    },
    deep_clone = function(name, value) {
      if (is.environment(value)) {
        private$deep_clone_env(value)
      } else if (R6::is.R6(value)) {
        value$clone(deep = TRUE)
      } else {
        value
      }
    },
    deep_clone_env = function(x) {
      # internal function to execute it recursively
      envir_to <- new.env()
      for (n in ls(x, all.names = TRUE)) {
        if (is.environment(get(n, envir = x))) {
          assign(n, value = private$deep_clone_env(get(n, envir = x)), envir = envir_to)
        } else {
          assign(n, value = get(n, envir = x), envir = envir_to)
        }
      }
      envir_to
    },
    is_empty = function() {
      isTRUE(length(private$id) == 0)
    },
    id_exists = function(id) {
      checkmate::assert_string(id)
      !private$is_empty() && isTRUE(id %in% private$id)
    },
    eval_id = function(id = character(0)) {
      if (is.null(id) || length(id) == 0) {
        id <- private$get_latest_id()
      }
      stopifnot(checkmate::test_string(id) && private$id_exists(id))

      if (!private$any_remaining()) {
        # Allow the latest element to be re-evaluated an infinite number of times.
        warning("All chunks were already evaluated The last evaluated chunk result is returned.")
        return(private$latest_result)
      } else if (!identical(id, utils::head(private$id_remaining(), 1))) {
        stop("Error in chunks stack execution. Execution was not performed in the right order.")
      }

      private$latest_result <- private$get_chunk(id)$eval(envir = private$envir)

      idx <- private$get_idx(id)
      private$is_evaluated[idx] <- TRUE
      private$is_remaining[idx] <- FALSE
      private$set_is_error(private$get_chunk(id)$is_errors(), idx)
      private$set_is_warning(private$get_chunk(id)$is_warnings(), idx)
      private$set_is_message(private$get_chunk(id)$is_messages(), idx)

      return(private$latest_result)
    },
    get_rcode_id = function(id = character(0)) {
      if (is.null(id) || length(id) == 0) {
        id <- private$get_latest_id()
      }
      checkmate::assert_string(id)
      stopifnot(private$id_exists(id))

      return(private$get_chunk(id)$get_rcode(envir = private$envir))
    },
    get_warnings_id = function(id = character(0)) {
      if (is.null(id) || length(id) == 0) {
        id <- private$get_latest_id()
      }
      checkmate::assert_string(id)
      stopifnot(private$id_exists(id))

      return(private$get_chunk(id)$get_warnings())
    },
    get_messages_id = function(id = character(0)) {
      if (is.null(id) || length(id) == 0) {
        id <- private$get_latest_id()
      }
      checkmate::assert_string(id)
      stopifnot(private$id_exists(id))

      return(private$get_chunk(id)$get_messages())
    },
    set_is_message = function(new_messages, idx = length(private$is_message) + 1) {
      if (is.null(idx)) {
        private$is_message <- new_messages
        private$reactive_summary$msgs <- new_messages
      } else {
        private$is_message[idx] <- new_messages
        isolate(private$reactive_summary$msgs[idx] <- new_messages)
      }
    },
    set_is_warning = function(new_warnings, idx = length(private$is_warning) + 1) {
      if (is.null(idx)) {
        private$is_warning <- new_warnings
        private$reactive_summary$warnings <- new_warnings
      } else {
        private$is_warning[idx] <- new_warnings
        isolate(private$reactive_summary$warnings[idx] <- new_warnings)
      }
    },
    set_is_error = function(new_error, idx = length(private$is_error) + 1) {
      if (is.null(idx)) {
        private$is_error <- new_error
        private$reactive_summary$errors <- new_error
      } else {
        private$is_error[idx] <- new_error
        isolate(private$reactive_summary$errors[idx] <- new_error)
      }
    }
  )
)


# Helper Functions ----

get_session_object <- function() {
  if (shiny::isRunning()) {
    getDefaultReactiveDomain()
  } else {
    if ("session" %in% ls(envir = .GlobalEnv)) {
      return(get("session", envir = .GlobalEnv))
    } else {
      stop("Cannot find 'session' object in .GlobalEnv.")
    }
  }
}

#' Gets chunks object
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param session optional, (\code{ShinySession})\cr
#'  \code{shiny} session object, if missing then
#'  \link[shiny]{getDefaultReactiveDomain} is used.
#' @param parent_idx optional (\code{integer})\cr
#'   from which shiny parent environment chunks object should be taken.
#'
#' @return \code{chunks} object
#' @export
get_chunks_object <- function(session = get_session_object(), parent_idx = 0L) {
  checkmate::assert_integer(parent_idx, lower = 0L, len = 1, any.missing = FALSE)
  target_name <- session$ns(character(0))
  if (parent_idx > 0) {
    target_name <- gsub(
      pattern = sprintf("(%1$s*?[^%1$s]+){%2$s}$", ns.sep, parent_idx), # nolint
      replacement = "",
      x = target_name,
      perl = TRUE
    )
  }
  session$userData[[target_name]]$chunks
}


clone_env <- function(envir_from, envir_to) {
  checkmate::assert_environment(envir_from)
  checkmate::assert_environment(envir_to)

  for (n in ls(envir_from, all.names = TRUE)) {
    assign(n, value = get(n, envir = envir_from), envir = envir_to)
  }

  invisible(NULL)
}

# Accessor and Modification function in a reactive environment ----

#' Pushes a code chunk for global chunks
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param expression (\code{call}) Expression that contains a function call.
#' @param id optional, (\code{character}) ID given for the code chunk
#' @param chunks optional, (\code{chunks}) object.
#'   If not provided then automatic \code{chunks} object detection is run via \link{get_chunks_object}
#'
#' @return Nothing, just add the chunk to the \code{chunks} argument
#'
#' @export
#'
#' @examples
#' all_chunks <- chunks$new()
#' chunks_push(call("as.character", x = 3), "tbl", chunks = all_chunks)
#'
#' x <- 3
#' chunks_push(quote(as.character(x)), "tbl2", chunks = all_chunks)
chunks_push <- function(expression,
                        id = NULL,
                        chunks = get_chunks_object()) {
  checkmate::assert_string(id, null.ok = TRUE)
  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }

  chunks$push(x = expression, id = id)
}

#' Pushes a chunks stack to global chunks
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams chunks_push
#' @param x (\code{chunks}) stack object
#' @param overwrite optional, (\code{logical}) Whether to ignore conflicts
#'  between the two chunk environments and use the added one as default
#'
#' @return Nothing, just add the chunk to the \code{chunks} argument
#'
#' @export
#'
#' @examples
#' chunks_object <- chunks$new()
#' chunks_push(bquote(x <- 1), chunks = chunks_object)
#'
#' chunks_object2 <- chunks$new()
#' chunks_push(bquote(y <- 1), chunks = chunks_object2)
#'
#' chunks_push_chunks(chunks_object2, chunks = chunks_object)
#'
#' chunks_get_rcode(chunks_object)
chunks_push_chunks <- function(x,
                               overwrite = FALSE,
                               chunks = get_chunks_object()) {
  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }

  return(chunks$push_chunks(x = x, overwrite = overwrite))
}

#' Pushes a merged_dataset to chunks
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams chunks_push
#' @param x (\code{list}) outcome of \code{data_merge_srv}
#'
#' @export
chunks_push_data_merge <- function(x, chunks = get_chunks_object()) {
  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }
  checkmate::assert_names(names(x), must.include = "chunks")

  chunks_push_chunks(x = x$chunks, chunks = chunks, overwrite = FALSE)
}

#' Pushes a code comment chunk for global chunks
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams chunks_push
#' @param comment (code{character}) Comment to be inserted into the Show-R code.
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#' all_chunks <- chunks$new()
#' chunks_push_comment("this is a comment", chunks = all_chunks)
#'
#' chunks_get_rcode(chunks = all_chunks) == "# this is a comment"
chunks_push_comment <- function(comment,
                                chunks = get_chunks_object()) {
  checkmate::assert_string(comment)
  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }

  return(chunks$push(x = comment))
}

#' Adds an empty line to the code chunks
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams chunks_push
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#' all_chunks <- chunks$new()
#' chunks_push_new_line(chunks = all_chunks)
#'
#' chunks_get_rcode(chunks = all_chunks) == " "
chunks_push_new_line <- function(chunks = get_chunks_object()) {
  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }

  return(chunks$push(x = ""))
}

#' Evaluates all remaining chunks
#'
#' @description `r lifecycle::badge("stable")`
#'
#' You can evaluate all remaining chunks of chunks being setup in the shiny session (see [get_chunks_object()]).
#' The value of the last chunk being evaluated will be returned.
#'
#' This method should not be evaluated multiple times, else it throws a warning. This is due to the
#' fact that multiple evaluations of the following second code snippet is dangerous:
#' \itemize{
#'   \item{1 }{a <- 2}
#'  \item{2 }{a <- a + 1}
#' }
#' \code{eval_chunks} evaluates code just once and warn user if one tries to evaluate multiple times.
#'
#' @inheritParams chunks_push
#'
#' @export
chunks_eval <- function(chunks = get_chunks_object()) {
  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }
  return(chunks$eval())
}

#' Resets the evaluation of the chunks.
#'
#' Keep code pieces.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams chunks_push
#' @param overwrite (\code{logical}) Whether to use values from the current environment
#'   or just use a new one
#' @param envir (\code{environment}) An environment to use overwriting. Only used if
#'   \code{overwrite} is \code{TRUE}
#'
#' @export
chunks_uneval <- function(chunks = get_chunks_object(), overwrite = FALSE, envir = parent.frame()) {
  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }
  return(chunks$uneval(overwrite = overwrite))
}

#' Evaluates all remaining chunks and validate if error
#'
#' @description `r lifecycle::badge("stable")`
#'
#' You can evaluate all remaining chunks of chunks being setup in the shiny session (see [get_chunks_object()]).
#' The value of the last chunk being evaluated will be returned.
#'
#' @inheritParams chunks_push
#'
#' @export
chunks_safe_eval <- function(chunks = get_chunks_object()) {
  checkmate::assert_class(chunks, "chunks")
  res <- chunks$eval()
  chunks$validate_is_ok()
  return(res)
}

#' Returns the R-Code from a `chunks` object
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This function returns a list of the R-Code that reproduces all currently registered chunks
#' inside the chunk stack.
#'
#' @inheritParams chunks_push
#'
#' @return The R code stored inside the chunks that can be used to reproduce the value of the evaluation
#'    of all code chunks.
#'
#' @export
chunks_get_rcode <- function(chunks = get_chunks_object()) {
  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }
  return(chunks$get_rcode())
}

#' Gets warnings from the \code{chunks} object
#'
#' @description `r lifecycle::badge("stable")`
#' This function returns a list of all the warnings encountered during evaluation of the code
#' in the \code{chunks} object.
#'
#' @inheritParams chunks_push
#'
#' @export
chunks_warnings <- function(chunks = get_chunks_object()) {
  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }
  return(chunks$get_warnings())
}

#' Gets messages from the \code{chunks} object
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This function returns a list of all the messages encountered during evaluation of the code
#' in the \code{chunks} object.
#'
#' @inheritParams chunks_push
#'
#' @export
chunks_messages <- function(chunks = get_chunks_object()) {
  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }
  return(chunks$get_messages())
}

#' Allows using chunks in the global environment of a teal app
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param new_chunks optional, (\code{chunks_stack}) new object to initialize with
#' @param session optional, (\code{ShinySession}) \code{shiny} session object.
#'
#' @return nothing, it modifies shiny session object
#'
#' @export
#'
#'
#' @references chunks
init_chunks <- function(new_chunks = chunks$new(), session = get_session_object()) {
  session$userData[[session$ns(character(0))]]$chunks <- "A"
  suppressWarnings(rm(envir = session$userData, list = session$ns(character(0))))
  session$userData[[session$ns(character(0))]]$chunks <- new_chunks # nolint
  return(invisible(NULL))
}

#' Overwrites chunks object in session
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x (\code{chunks}) \link{chunks}-object to be used inside the current
#'   session.
#' @param session optional, (\code{ShinySession}) \code{shiny} session object,
#'   if missing then \link[shiny]{getDefaultReactiveDomain} is used.
#'
#' @return nothing, it modifies shiny session object
#'
#' @export
overwrite_chunks <- function(x = chunks$new(envir = parent.frame()), session = get_session_object()) {
  if (!inherits(x, "chunks")) {
    stop("No chunks object provided for 'overwrite_chunks' in argument 'x'.")
  }

  if (is.null(get_chunks_object(session = session))) {
    stop("You cannot overwrite chunks without initializing them by 'init_chunks()'.")
  }

  session$userData[[session$ns(character(0))]]$chunks <- x # nolint

  message("Chunks successfully overwritten")

  return(invisible(NULL))
}

#' Resets the current chunk list
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Empty the chunk list and the remaining and evaluated list.
#'
#' @inheritParams chunks_push
#' @param envir (\code{environment}) The environment to hand over to the chunks.
#'
#' @return nothing, it modifies shiny session object
#'
#' @export
chunks_reset <- function(envir = parent.frame(),
                         chunks = get_chunks_object()) {
  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }
  chunks$reset(envir = envir)

  return(invisible(NULL))
}

#' Get variable from chunk environment
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams chunks_push
#' @param var (\code{character}) variable name
#'
#' @return variable from chunks environment
#'
#' @export
chunks_get_var <- function(var,
                           chunks = get_chunks_object()) {
  checkmate::assert_string(var)
  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }

  return(chunks$get(var = var))
}

#' Check chunks status (i.e. evaluated and no errors)
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams chunks_push
#'
#' @return (\code{logical}) flag whether everything is good
#'
#' @export
chunks_is_ok <- function(chunks = get_chunks_object()) {
  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }
  return(chunks$is_ok())
}

#' Get chunks evaluation message
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams chunks_push
#'
#' @return (\code{character}) chunks evaluation message
#'
#' @export
chunks_get_eval_msg <- function(chunks = get_chunks_object()) {
  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }
  return(chunks$get_eval_msg())
}

#' Raise shiny validate error if chunks status is not ok
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams chunks_push
#' @param msg optional, (\code{character}) custom error message, if \code{NULL} then default error message is used
#'
#' @return shiny validation error if chunks are not ok
#'
#' @export
#'
#' @references chunks_is_ok
chunks_validate_is_ok <- function(msg = NULL,
                                  chunks = get_chunks_object()) {
  checkmate::assert_string(msg, null.ok = TRUE)
  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }

  chunks$validate_is_ok(msg)

  return(invisible(NULL))
}

#' Raise shiny validate error if variable is not of a certain class
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams chunks_validate_is_ok
#' @param var (\code{character}) variable name
#' @param class (\code{character}) class name
#'
#' @return shiny validation error if variable is not of certain class
#'
#' @export
chunks_validate_is <- function(var,
                               class,
                               msg = NULL,
                               chunks = get_chunks_object()) {
  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }

  chunks$validate_is(var, class, msg)

  return(invisible(NULL))
}

#' Raise shiny validate error if chunks status is not ok or variable is not of a certain class
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams chunks_validate_is
#'
#' @return shiny validation error if conditions are met
#'
#' @export
#'
#' @references chunks_validate_is_ok chunks_validate_is
chunks_validate_all <- function(var,
                                class,
                                msg = NULL,
                                chunks = get_chunks_object()) {
  checkmate::assert_string(var)
  checkmate::assert_string(class)
  checkmate::assert_string(msg, null.ok = TRUE)
  if (!inherits(chunks, "chunks")) {
    stop("Provided chunks are not of class chunks. Make sure to use init_chunks() in the module's server function.")
  }

  chunks$validate_all(var, class, msg)

  return(invisible(NULL))
}


#' Executes validate statements on custom expressions that are evaluated inside a chunks object's environment
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams chunks_validate_is
#' @param x (\code{language}) an expression that evaluates to \code{TRUE} or \code{FALSE} inside \code{chunks}
#'   environment
#'
#' @return shiny validation error if x evaluates to FALSE in chunks environment
#'
#' @export
chunks_validate_custom <- function(x,
                                   msg = NULL,
                                   chunks = get_chunks_object()) {
  stopifnot(`x is not an expression` = is.language(x))
  stopifnot(`chunks is not a chunks object` = inherits(chunks, "chunks"))
  stopifnot(
    `msg is not a character or its length is more than 1` = checkmate::test_string(msg, null.ok = TRUE)
  )
  res <- eval(x, envir = chunks$info()$envir)
  if (!is.logical(res)) {
    stop(paste("x did not return a logical value when evaluated in chunks environment, but", typeof(res), "instead"))
  }
  if (any(is.na(res))) {
    stop(paste("x returned NA value(s) when evaluated in chunks environment"))
  }
  if (is.null(msg)) msg <- paste(deparse(x), "is not all TRUE")
  validate(need(all(res), msg))
}


#' Deep clones a chunk object
#' @description `r lifecycle::badge("stable")`
#' @inheritParams chunks_push
#'
#' @details use this function if you need to copy a `chunks` object as this
#'   function makes sure all associated `environments` a deep copied (i.e. are independent
#'  of the original object)
#' @return a deep copy of `chunks`
#' @export
#' @examples
#' x_chunk <- chunks$new()
#' chunks_push(expression(y <- 1), chunks = x_chunk)
#'
#' # A copy of x_chunk which does not share the same environment
#' x_chunk_copy <- chunks_deep_clone(x_chunk)
#'
#' # Add expression only into x_chunk
#' chunks_push(chunks = x_chunk, expression = expression(y <- 2 * y))
#'
#' # Get R code from both chunks, note it is different
#' chunks_get_rcode(x_chunk)
#' chunks_get_rcode(x_chunk_copy)
#'
#' # Evaluate both chunks, note the result is different
#' chunks_safe_eval(x_chunk)
#' chunks_safe_eval(x_chunk_copy)
chunks_deep_clone <- function(chunks = get_chunks_object()) {
  checkmate::assert_class(chunks, "chunks")
  return(chunks$clone(deep = TRUE))
}
