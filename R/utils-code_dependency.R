#' Create Object Dependencies Structure Within Parsed Code
#'
#' @description This function constructs a dependency structure that identifies the relationships between objects in
#' parsed code. It helps you understand which objects are needed to recreate a specific object.
#'
#' @details This function assumes that object relationships are established using the `<-`, `=`, or `->` assignment
#' operators. It does not support other object creation methods like `assign` or `<<-`, nor non-standard-evaluation
#' methods. To specify relationships between side-effects and objects, you can use the comment tag
#' `# @linksto object_name` at the end of a line where the side-effect occurs.
#'
#' @param code An `expression` with `srcref` attribute or a `character` with the code.
#' @param object_names (`character(n)`) A vector containing the names of existing objects.
#'
#' @return A `list` with three components:
#' - `occurrence`: A named `list` where object names are the names of existing objects, and each element is a numeric
#' vector indicating the calls in which the object appears.
#' - `cooccurrence`: A `list` of the same length as the number of calls in `parsed_code`
#' (`parsed_code = parse(text = code)` for code input as `character` and `parsed_code = code` for expression input.
#' It contains `NULL` values if there is no co-occurrence between objects or a `character` vector indicating the
#' co-occurrence of objects in a specific `parsed_code` call element. If it's a character vector, the first element is
#' the name of the dependent object, and the rest are the influencing objects.
#' - `effects`: A named `list` where object names are the names of existing objects, and each element is a numeric
#' vector indicating which calls have an effect on that object. If there are no side-effects pointing at an object,
#' the element is `NULL`.
#'
#'
#' @keywords internal
#'
code_dependency <- function(code, object_names) {
  checkmate::assert_multi_class(code, classes = c("character", "expression"))
  checkmate::assert_character(object_names, null.ok = TRUE)

  if (is.expression(code)) {
    if (!is.null(attr(code, "srcref"))) {
      parsed_code <- code
    } else {
      stop("The 'expression' code input does not contain 'srcref' attribute.")
    }
  }

  if (is.character(code)) {
    parsed_code <- parse(text = code, keep.source = TRUE)
  }

  pd <- utils::getParseData(parsed_code)

  calls_pd <- lapply(pd[pd$parent == 0, "id"], get_children, pd = pd)

  occurrence <- lapply(sapply(object_names, detect_symbol, calls_pd = calls_pd, simplify = FALSE), which)

  cooccurrence <- lapply(
    calls_pd,
    function(x) {
      sym_cond <- which(x$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL") & x$text %in% object_names)
      sym_form_cond <- which(x$token == "SYMBOL_FORMALS" & x$text %in% object_names)
      sym_cond <- sym_cond[!x[sym_cond, "text"] %in% x[sym_form_cond, "text"]]

      object_ids <- x[sym_cond, "id"]
      dollar_ids <- x[x$"token" %in% c("'$'", "'@'"), "id"]
      after_dollar <- object_ids[(object_ids - 2) %in% dollar_ids]
      sym_cond <- setdiff(sym_cond, which(x$id %in% after_dollar))

      if (length(sym_cond) >= 2) {
        ass_cond <- grep("ASSIGN", x$token)
        text <- unique(x[sort(c(sym_cond, ass_cond)), "text"])

        if (text[1] == "->") {
          rev(text[-1])
        } else {
          text[-1]
        }
      }
    }
  )

  side_effects <- grep("@linksto", pd[pd$token == "COMMENT", "text"], value = TRUE)
  check_effects <-
    if (length(side_effects) > 0) {
      affected <-
        unlist(strsplit(sub("\\s*#\\s*@linksto\\s+", "", side_effects), "\\s+"))

      union(object_names, affected)
    } else {
      object_names
    }

  effects <- sapply(
    check_effects,
    function(x) {
      maxid <- suppressWarnings(max(occurrence[[x]]))
      return_code_for_effects(
        x,
        calls_pd = calls_pd,
        occur = suppressWarnings(lapply(occurrence, function(x) setdiff(x, maxid:max(maxid, max(x))))),
        cooccur = cooccurrence,
        eff = NULL
      )
    },
    simplify = FALSE
  )

  list(
    occurrence = occurrence,
    cooccurrence = cooccurrence,
    effects = effects
  )
}

#' @title Get child calls within `getParseData()` object
#' @param pd (`data.frame`) A result of `utils::getParseData()`.
#' @param parent Object parent id in `utils::getParseData()`.
#' @return Row `bounded` `utils::getParseData()` of all elements of a call pointing to a `parent` id.
#' @keywords internal
get_children <- function(pd, parent) {
  idx_children <- abs(pd$parent) == parent
  children <- pd[idx_children, c("token", "text", "id")]
  if (nrow(children) == 0) {
    return(NULL)
  }

  if (parent > 0) {
    do.call(rbind, c(list(children), lapply(children$id, get_children, pd = pd)))
  }
}

#' @title Detects `"SYMBOL"` tokens for row `bounded` `getParseData()` structure
#' @param object `character` containing the name of the object
#' @param calls_pd A `list` of `data.frame`s, which is a result of `get_children(utils::getParseData(), parent = 0)`
#' applied on `parse(text = code, keep.source = TRUE)` at `code_dependency(code)`.
#' @return A `logical` vector pointing in which elements of `pd` the `SYMBOL` token row has `object` in text column
#' @keywords internal
detect_symbol <- function(object, calls_pd) {
  vapply(
    calls_pd,
    function(call) {
      is_symbol <-
        any(call[call$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL"), "text"] == object)

      is_formal <- used_in_function(call, object)

      object_ids <- call[call$text == object, "id"]
      dollar_ids <- call[call$"token" %in% c("'$'", "'@'"), "id"]
      after_dollar <- object_ids[(object_ids - 2) %in% dollar_ids]
      object_ids <- setdiff(object_ids, after_dollar)

      is_symbol & !is_formal & length(object_ids) > 0
    },
    logical(1)
  )
}

#' @title Whether an object is used inside a function within a call
#' @param call An element of `calls_pd` list used in `detect_symbol`.
#' @param object A character with object name.
#' @return A `logical(1)`.
#' @keywords internal
used_in_function <- function(call, object) {
  if (any(call[call$token == "SYMBOL_FORMALS", "text"] == object) && any(call$token == "FUNCTION")) {
    object_sf_ids <- call[call$text == object & call$token == "SYMBOL", "id"]
    function_start_id <- call[call$token == "FUNCTION", "id"]
    all(object_sf_ids > function_start_id)
  } else {
    FALSE
  }
}

#' Return the lines of code needed to reproduce the object.
#' @return `numeric` vector indicating which lines of `parsed_code` calls are required to build the `object`
#'
#' @param object `character` with object name
#' @param occur result of `code_dependency()$occurrence`
#' @param cooccur result of `code_dependency()$cooccurrence`
#' @param eff result of `code_dependency()$effects`
#' @param parent `NULL` or `numeric` vector - in a recursive call, it is possible needed to drop parent object
#' indicator to omit dependency cycles
#'
#' @return A `numeric` vector with number of lines of input `pd` to be returned.
#'
#' @keywords internal
return_code <- function(object, occur, cooccur, eff, parent = NULL) {
  if (all(unlist(lapply(occur, length)) == 0)) {
    return(NULL)
  }

  influences <- vapply(cooccur, match, integer(1L), x = object)
  where_influences <- which(influences > 1L)
  object_influencers <- which(influences == 1L)

  object_influencers <- setdiff(object_influencers, parent)

  lines <- setdiff(occur[[object]], where_influences)

  if (length(object_influencers) == 0) {
    return(sort(unique(lines)))
  } else {
    for (idx in object_influencers) {
      influencer_names <- cooccur[[idx]][-1]

      influencer_lines <-
        unlist(
          lapply(
            influencer_names,
            return_code,
            occur = suppressWarnings(lapply(occur, function(x) setdiff(x, idx:max(idx, max(x))))),
            cooccur = cooccur[1:idx],
            parent = where_influences,
            eff = eff
          )
        )

      influencer_effects_lines <- unlist(eff[influencer_names])
      lines <- c(lines, influencer_lines, influencer_effects_lines)
    }
    sort(unique(lines))
  }
}

#' Return the lines of code needed to reproduce the side-effects having an impact on the object.
#' @return `numeric` vector indicating which lines of `parsed_code` calls are required to build the side-effects having
#' and impact on the `object`
#'
#' @param object `character` with object name
#' @param calls_pd A `list` of `data.frame`s, which is a result of `get_children(utils::getParseData(), parent = 0)`
#' applied on `parse(text = code, keep.source = TRUE)` at `code_dependency(code)`.
#' @param occur result of `code_dependency()$occurrence`
#' @param cooccur result of `code_dependency()$cooccurrence`
#'
#' @return A `numeric` vector with number of lines of input `pd` to be returned for effects.
#'
#' @keywords internal
return_code_for_effects <- function(object, calls_pd, occur, cooccur, eff) {
  symbol_effects_names <-
    unlist(
      lapply(
        calls_pd,
        function(x) {
          com_cond <-
            x$token == "COMMENT" & grepl("@linksto", x$text) & grepl(paste0("[\\s]*", object, "[\\s$]*"), x$text)

          # Make sure comment id is not the highest id in the item.
          # For calls like 'options(prompt = ">") # @linksto ADLB',
          # 'options(prompt = ">")' is put in a one item
          # and '# @linksto ADLB' is the first element of the next item.
          # This is tackled in B.


          if (!com_cond[1] & sum(com_cond) > 0) {
            # A.
            x[x$token == "SYMBOL", "text"]
          } else if (com_cond[1] & sum(com_cond[-1]) > 0) {
            # B.
            x <- x[-1, ]
            x[x$token == "SYMBOL", "text"]
          }
        }
      )
    )

  commented_calls <- vapply(
    calls_pd,
    function(x) any(x$token == "COMMENT" & grepl("@linksto", x$text)),
    logical(1)
  )

  symbol_effects_lines <-
    unlist(
      lapply(
        symbol_effects_names,
        function(x) {
          code <- return_code(x, occur = occur, cooccur = cooccur, eff = eff)
          if (is.null(code)) {
            # Below is just used for comments with @linksto.
            intersect(which(detect_symbol(x, calls_pd)), which(commented_calls))
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
        calls_pd,
        function(x) {
          com_cond <-
            x$token == "COMMENT" & grepl("@linksto", x$text) & grepl(paste0("[\\s]*", object, "[\\s$]*"), x$text)

          # Work out the situation when comment id is the highest id in the item.
          # For calls like 'options(prompt = ">") # @linksto ADLB',
          # 'options(prompt = ">")' is put in a one item
          # and '# @linksto ADLB' is the first element of the next item.

          com_cond[1]
        }
      )
    )

  side_effects_lines <- which(side_effects_names) - 1

  sort(unique(c(symbol_effects_lines, side_effects_lines)))
}

#' Return the lines of code (with side-effects) needed to reproduce the object
#' @return `character` vector of elements of `code` calls that were required to build the side-effects and
#' influencing objects having and impact on the `object`
#'
#' @param code An `expression` with `srcref` attribute or a `character` with the code.
#' @param names A `character(n)` with object names.
#' @keywords internal
get_code_dependency <- function(code, names) {
  checkmate::assert_multi_class(code, classes = c("character", "expression"))
  checkmate::assert_character(names)

  if (is.expression(code)) {
    if (!is.null(attr(code, "srcref"))) {
      parsed_code <- code
    } else {
      stop("The 'expression' code input does not contain 'srcref' attribute.")
    }
  }

  if (is.character(code)) {
    parsed_code <- parse(text = code, keep.source = TRUE)
  }

  pd <- utils::getParseData(parsed_code)

  symbols <- unique(pd[pd$token == "SYMBOL", "text"])

  if (!all(names %in% symbols)) {
    warning(
      "Objects not found in 'qenv' environment: ",
      toString(setdiff(names, symbols))
    )
  }

  code_dependency <- code_dependency(parsed_code, symbols)

  lines <-
    sapply(names, function(name) {
      object_lines <-
        return_code(
          name,
          occur = code_dependency$occurrence,
          cooccur = code_dependency$cooccurrence,
          eff = code_dependency$effects
        )

      effects_lines <- code_dependency$effects[[name]]
      c(object_lines, effects_lines)
    },
    simplify = FALSE
    )

  object_lines_unique <- sort(unique(unlist(lines)))

  as.character(parsed_code)[object_lines_unique]
}
