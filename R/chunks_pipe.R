chunk_link <- R6::R6Class(
  classname = "chunklink",
  public = list(
    id = NA,
    initialize = function(id = NULL) {
      stopifnot(!is.null(id))
      self$id <- id
    },
    eval = function(chunks = get_chunks_object()) {
      chunks_eval(chunks = chunks)
    }
  )
)

pipe_chunk <- function() {
  function(lhs, rhs) {
    pipe <- gsub(as.character(match.call()[[1]]), pattern = "%", replacement = "")

    chunk_name <- as.character(match.call()[[2]])

    right_hand_side <- match.call()[[3]]
    if (is.name(right_hand_side)) {
      right_hand_side <- get(as.character(right_hand_side), envir = parent.frame())
    }

    stopifnot(is.call(right_hand_side))
    stopifnot(length_check(chunk_name))

    if (!shiny::isRunning()) {
      session <- parent.frame()$session
      chunks_handler <- session$userData[[session$ns(character(0))]]$chunks
    } else {
      chunks_handler <- get_session_object()$userData[[get_session_object()$ns(character(0))]]$chunks
    }

    p <- parent.frame()

    if (is.call(match.call()[[2]])) {
      eval(match.call()[[2]], envir = p)
    }

    chunks_push(
      expression = right_hand_side,
      chunks = chunks_handler
    )

    p[[chunk_name]] <- chunk_link$new(chunks_handler$.__enclos_env__$private$get_latest_id())
  }
}

#' Set a code chunk
#'
#' @description `r lifecycle::badge("stable")`
#'
#' You can use these two functions to set code chunks within a reactive inside
#' the server function of a teal module. You can use `%chunk>%` if you
#' want to simply set the call into the chunk. You can use `<%chunk>%` if you
#' also want to assign the left hand side value to the result of this expression
#'
#' @examples
#'
#' session <- new.env()
#' session$userData <- new.env()
#' session$ns <- function(x) {
#'   if (length(x) == 0) {
#'     "id"
#'   } else {
#'     paste0("id", x, sep = "-")
#'   }
#' }
#' session$userData[[session$ns(character(0))]]$chunks <- chunks$new()
#'
#' x <- 5
#' session$userData[[session$ns(character(0))]]$chunks$reset()
#' myval %<chunk% sum(x, 3)
#'
#' mysum %<chunk% sum(x, 5)
#'
#' chunks_eval(chunks = session$userData[[session$ns(character(0))]]$chunks)
#' #>[1] 10
#'
#' cat(
#'   paste(
#'     session$userData[[session$ns(character(0))]]$chunks$get_rcode(),
#'     collapse = "\n"
#'   )
#' )
#' # x + 3
#' # sum(x, 5)
#' @name chunkpipe
#'
#' @export
#' @usage NULL
`%<chunk%` <- pipe_chunk() # nolint

length_check <- function(x) {
  length(x) > 0
}
