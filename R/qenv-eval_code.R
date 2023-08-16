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

  object_names <- {d <- new.env(); eval(parse(text = code), envir = d); ls(d)}

  detect_symbol <- function(obj, pd = calls_pd) {
    unlist(
      lapply(
        pd,
        function(call) {
          any(call[call$token == 'SYMBOL', 'text'] == obj)
        }
      )
    )
  }

  occurence <- lapply(lapply(object_names, detect_symbol), which)

  names(occurence) <- object_names

  cooccurence <- lapply(
    calls_pd,
    function(x) {
      sym_cond <- which(x$token == 'SYMBOL' & x$text %in% object_names)
      if (length(sym_cond) >= 2) {
        ass_cond <- grep('ASSIGN', x$token)
        text <- unique(x[sort(c(sym_cond, ass_cond)), 'text'])

        if (text[1] == "->") {
          rev(text[-1])
        } else {
          text[-1]
        }

      }
    }
  )

  return_code <- function(object, occur = occurence, cooccur = cooccurence, parent = NULL){

    influences <-
      lapply(
        cooccur,
        function(x)
          if (!is.null(x) && object %in% x[-1]) {
            TRUE
          } else if (!is.null(x) && object == x[1]){
            FALSE
          }
      )

    where_influences   <- which(unlist(lapply(influences, isTRUE)))
    object_influencers <- which(unlist(lapply(influences, isFALSE)))

    object_influencers <- setdiff(object_influencers, parent)

    lines <- setdiff(occur[[object]], where_influences)

    if (length(object_influencers) == 0) {
      return(sort(unique(lines)))
    } else {
      for(idx in object_influencers){

        # TRIM DOWN TO LINES ONLY NEEDED TO CREATE THE INITIAL OBJECT,
        # NOT TO ALL LINES OF THE INFLUENCER OBJECT.

        influencer_names <- cooccur[[idx]][-1]

        influencer_lines <-
          unlist(
            lapply(
              influencer_names,
              return_code,
              occur = occur,
              cooccur = cooccur[1:idx],
              parent = idx
            )
          )

        lines <- c(lines, influencer_lines)
      }
      sort(unique(lines))
    }

  }

  # pd_full <- getParseData(parsed_code, includeText = TRUE)
  # pd_full[pd_full$parent == 0, "text"]

  # DEAL WITH @effects

  return_code_for_effects <- function(object, pd = calls_pd, occur = occurence, cooccur = cooccurence) {

    symbol_effects_names <-
      unlist(
        lapply(
          pd,
          function(x) {
            com_cond <-
              x$token == 'COMMENT' & grepl('@effect', x$text) & grepl(paste0('[\\s]*', object, '[\\s$]*'), x$text)

            # Make sure comment id is not the highest id in the item.
            # For calls like 'options(prompt = ">") # @effect ADLB',
            # 'options(prompt = ">")' is put in a one item
            # and '# @effect ADLB' is the first element of the next item.
            # This is tackled in B.


            if (!com_cond[1] & sum(com_cond) > 0){
              # A.
              x[x$token == 'SYMBOL', 'text']
            } else if (com_cond[1] & sum(com_cond[-1]) > 0) {
              # B.
              x <- x[-1, ]
              x[x$token == 'SYMBOL', 'text']
            }
          }
        )
      )

    commented_calls <- vapply(pd,
      function(x) any(x$token == "COMMENT" & grepl("@effect", x$text)),
      FUN.VALUE = logical(1)
    )

    symbol_effects_lines <-
      unlist(
        lapply(
          symbol_effects_names,
          function(x) {
            code <- return_code(x, occur, cooccur) # QUESTION: SHOULD cooccur BE TRIMMED like it happens in return_code()?
            if (is.null(code)) {
              # Extract lines for objects that were used, but never created.
              # Some objects like 'iris' or 'mtcars' are pre-assigned in the session.
              # Below is just used for comments with @effect.
              # if (!object %in% names(occur)) {
              intersect(which(detect_symbol(x, pd)), which(commented_calls))
              # }
            } else {
              code
            }
          }
        )
      )

    # When commet_id is the highest id in the item - take previous item.
    side_effects_names <-
      unlist(
        lapply(
          pd,
          function(x) {
            com_cond <-
              x$token == 'COMMENT' & grepl('@effect', x$text) & grepl(paste0('[\\s]*', object, '[\\s$]*'), x$text)

            # Work out the situation when comment id is the highest id in the item.
            # For calls like 'options(prompt = ">") # @effect ADLB',
            # 'options(prompt = ">")' is put in a one item
            # and '# @effect ADLB' is the first element of the next item.

            com_cond[1]
          }
        )
      )

    side_effects_lines <- which(side_effects_names) - 1

    sort(unique(c(symbol_effects_lines, side_effects_lines)))

  }

  object <- 'ADLB'
  object_lines <- sort(unique(c(return_code(object), return_code_for_effects(object))))
  srcref[[object_lines]]


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
