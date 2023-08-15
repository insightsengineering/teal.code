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
setGeneric("eval_code", function(object, code) standardGeneric("eval_code"))

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
    # Using withCallingHandlers to capture ALL warnings and messages.
    # Using tryCatch to capture the FIRST error and abort further evaluation.
    x <- withCallingHandlers(
      tryCatch(
        {
          eval(code_line, envir = object@env)
          NULL
        },
        error = function(e) {
          errorCondition(
            message = sprintf(
              "%s \n when evaluating qenv code:\n%s",
              .ansi_strip(conditionMessage(e)),
              paste(format_expression(code), collapse = "\n")
            ),
            class = c("qenv.error", "try-error", "simpleError"),
            trace = object@code
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
  #eval_code(object, code = parse(text = code, keep.source = FALSE))
  parsed_code <- parse(text = code)
  srcref <- attr(parsed_code, 'srcref')
  pd <- getParseData(parsed_code)

  get_children <- function(pd, parent) {
    idx_children <- abs(pd$parent) == parent
    children <- pd[idx_children, c("token", "text", "id")]
    if (nrow(children) == 0) {
      return(NULL)
    }

    if (parent > 0) {
      rbind(
        children,
        do.call(rbind, lapply(children$id, get_children, pd = pd))
      )
    }
  }

  calls_pd <- lapply(pd[pd$parent == 0, "id"], get_children, pd = pd)

  commented_calls <- vapply(
    calls_pd,
    function(x) any(x$token == "COMMENT" & grepl("@effect", x$text)),
    FUN.VALUE = logical(1)
  )
  #calls_pd[commented_calls]

  object_names <- {d <- new.env(); eval(parse(text = code), envir = d); ls(d)}

  # may not be needed
  object_occurence <-
    lapply(
      lapply(
        object_names,
        function(obj) {
          unlist(
            lapply(
              calls_pd,
              function(call) {
                any(call[call$token == 'SYMBOL', 'text'] == obj)
              }
            )
          )
        }
      ),
      which
    )

  names(object_occurence) <- object_names

  object_cooccurence <- lapply(
    calls_pd,
    function(x) {
      sym_cond <- which(x$token == 'SYMBOL' & x$text %in% object_names)
      if (length(sym_cond) >= 2) {
        ass_cond <- grep('ASSIGN', x$token)
        unique(x[sort(c(sym_cond, ass_cond)), 'text'])
      }
    }
  )
  # TODO: double check if this is always length 3 (what if higher?)

  return_code <- function(object, src = srcref, object_o = object_occurence, object_co = object_cooccurence, skip = NULL){

    # TODO: can below two calls can be substituted with one?
    where_influences <-
      which(
        unlist(
          lapply(
            object_co,
            function(x)
              if (!is.null(x)) {
                (x[3] == object && x[1] %in% c("=", "<-")) || (x[2] == object && x[1] == "->")
              } else {
                FALSE
              }
            )
        )
      )

    object_influencers <-
      which(
        unlist(
          lapply(
            object_co,
            function(x)
              if (!is.null(x)) {
                (x[2] == object && x[1] %in% c("=", "<-")) || (x[3] == object && x[1] == "->")
              } else {
                FALSE
              }
          )
        )
      )
    object_influencers <- setdiff(object_influencers, skip)

    lines <- setdiff(object_o[[object]], where_influences)

    if (length(object_influencers) == 0) {
      return(sort(unique(lines)))
    } else {
      for(obj in object_influencers){

        # TRIM DOWN TO LINES ONLY NEEDED TO CREATE THE INITIAL OBJECT,
        # NOT TO ALL LINES OF THIS INFLUENCER OBJECT
        # TODO: rewrite to the current object_co structure
        co_pos <-
          object_co %>%
          filter((object1 == object & object2 == obj & direction == '1<-2') |
                   (object2 == object & object1 == obj & direction == '1->2')) %>%
          filter(position == max(position)) %>%
          pull(position)

        obj_code <- return_code(obj, src = src[1:co_pos], object_o = object_o, object_co = object_co, skip = object)

        lines <- c(lines, obj_code)
      }
      sort(unique(lines))
    }

  }
  # TODO: work on @effects

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
