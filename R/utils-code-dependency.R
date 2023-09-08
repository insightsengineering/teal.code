#' Create Object Dependencies Structure Within Parsed Code
#'
#' @description Build up ingredients needed to restore the code required to create a specific object.
#' @details The relation between objects is assumed to be passed by `<-`, `=` or `->` assignment operators. No other
#' object creation methods (like `assign`, or `<<-` or any non-standard-evaluation method) are supported. To specify
#' relations between side-effects and objects, use specific comment tag `# @effect object_name` at the end of the line
#' in which the side-effect appears. Check examples to see the usage.
#'
#' @param parsed_code (`expression`) result of `parse()`
#' @param envir `environment` in which to seek objects created during `eval` of `parsed_code`
#'
#' @return A `list` containing 3 elements
#' - `occurrence` - named `list` by object names with numeric vector as elements indicating calls in which object
#' appears.
#' - `cooccurrence` - `list` of the same length as number of calls in `parsed_code`, containing `NULL`s if there is no
#' co-occurrence between objects, or a `character` vector indicating co-occurrence of objects in specific `parsed_code`
#' call element. If a character vector, then the first element is the name of the dependent object, and the rest are the
#' influencing objects
#' - `effects` - named `list`  by object names with numeric vector as elements indicating which calls has effect on this
#' object, or NULL if there are no side-effects pointing at this object.
#'
#' @examples
#'
#' library(dplyr)
#' code <- '
#'   arm_mapping <- list(
#'     "A: Drug X" = "150mg QD",
#'     "B: Placebo" = "Placebo",
#'     "C: Combination" = "Combination"
#'   )
#'   color_manual <- c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C")
#'   # assign LOQ flag symbols: circles for "N" and triangles for "Y", squares for "NA"
#'   shape_manual <- c("N" = 1, "Y" = 2, "NA" = 0)
#'   ADSL <- goshawk::rADSL
#'   goshawk::rADLB-> ADLB
#'   iris2 <- iris # @effect ADLB ADSL
#'   var_labels <- lapply(ADLB, function(x) attributes(x)$label)
#'   iris3 <- iris'
#' code2 <- '
#'   ADLB <- ADLB %>%
#'     dplyr::mutate(AVISITCD = dplyr::case_when(
#'       AVISIT == "SCREENING" ~ "SCR",
#'       AVISIT == "BASELINE" ~ "BL",
#'       grepl("WEEK", AVISIT) ~
#'         paste(
#'           "W",
#'           trimws(
#'             substr(
#'               AVISIT,
#'               start = 6,
#'               stop = stringr::str_locate(AVISIT, "DAY") - 1
#'             )
#'           )
#'         ),
#'       TRUE ~ NA_character_
#'     )) %>%
#'     dplyr::mutate(AVISITCDN = dplyr::case_when(
#'       AVISITCD == "SCR" ~ -2,
#'       AVISITCD == "BL" ~ 0,
#'       grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
#'       TRUE ~ NA_real_
#'     )) %>%
#'     # use ARMCD values to order treatment in visualization legend
#'     dplyr::mutate(TRTORD = ifelse(grepl("C", ARMCD), 1,
#'                                   ifelse(grepl("B", ARMCD), 2,
#'                                          ifelse(grepl("A", ARMCD), 3, NA)
#'                                   )
#'     )) %>%
#'     dplyr::mutate(ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))])) %>%
#'     dplyr::mutate(ARM = factor(ARM) %>%
#'                     reorder(TRTORD)) %>%
#'     dplyr::mutate(
#'       ANRHI = dplyr::case_when(
#'         PARAMCD == "ALT" ~ 60,
#'         PARAMCD == "CRP" ~ 70,
#'         PARAMCD == "IGA" ~ 80,
#'         TRUE ~ NA_real_
#'       ),
#'       ANRLO = dplyr::case_when(
#'         PARAMCD == "ALT" ~ 20,
#'         PARAMCD == "CRP" ~ 30,
#'         PARAMCD == "IGA" ~ 40,
#'         TRUE ~ NA_real_
#'       )
#'     ) %>%
#'     dplyr::rowwise() %>%
#'     dplyr::group_by(PARAMCD) %>%
#'     dplyr::mutate(LBSTRESC = ifelse(
#'       USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'       paste("<", round(runif(1, min = 25, max = 30))), LBSTRESC
#'     )) %>%
#'     dplyr::mutate(LBSTRESC = ifelse(
#'       USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'       paste(">", round(runif(1, min = 70, max = 75))), LBSTRESC
#'     )) %>%
#'     ungroup()'
#'
#' code3 <- '
#'   attr(ADLB[["ARM"]], "label") <- var_labels[["ARM"]]
#'   attr(ADLB[["ANRHI"]], "label") <- "Analysis Normal Range Upper Limit"
#'   attr(ADLB[["ANRLO"]], "label") <- "Analysis Normal Range Lower Limit"
#'   mtcars # @effect ADLB
#'   options(prompt = ">") # @effect ADLB
#'
#'   # add LLOQ and ULOQ variables
#'   ADLB_LOQS<-goshawk:::h_identify_loq_values(ADLB)
#'   goshawk:::h_identify_loq_values(ADLB)->ADLB_LOQS
#'   ADLB = dplyr::left_join(ADLB, ADLB_LOQS, by = "PARAM")
#'   iris6 <- list(ADLB, ADLB_LOQS, ADSL)
#'   iris5 <- iris'
#'
#' d <- new.env()
#' old_code_dep <- code_dependency(parse(text = code), envir = d)
#' eval(parse(text = code), envir = d)
#' ls(d)
#' new_code_dep <- code_dependency(parse(text = code2), envir = d)
#' bind_code_dependency(old_code_dep, new_code_dep)
#'
#'
#' q1 <- teal.code:::new_qenv()
#' q1@code_dependency
#'
#' q2 <- teal.code::eval_code(q1, code = code)
#' q2@code
#' q2@code_dependency
#'
#' q3 <- teal.code::eval_code(q2, code = code2)
#' q3@code
#' q3@code_dependency
#'
#' q4 <- teal.code::eval_code(q3, code = code3)
#' q4@code
#' q4@code_dependency
#'
#' get_code(q2, deparse = FALSE, names = "ADLB")
#' get_code(q3, deparse = FALSE, names = "ADLB")
#' get_code(q4, deparse = FALSE, names = "ADLB")
#' get_code(q4, deparse = FALSE, names = "var_labels")
#' get_code(q4, deparse = FALSE, names = "ADSL")
#' get_code(q4, deparse = FALSE, names = c("var_labels", "ADSL"))
#' get_code(q4)
#'
#' @keywords internal
code_dependency <- function(parsed_code, envir = new.env()) {
  if (identical(parsed_code, expression())) {
    return(list())
  }

  pd <- getParseData(parsed_code)

  calls_pd <- lapply(pd[pd$parent == 0, "id"], get_children, pd = pd)

  object_names <- ls(envir)

  occurrence <- lapply(lapply(object_names, detect_symbol, pd = calls_pd), which)

  names(occurrence) <- object_names

  cooccurrence <- lapply(
    calls_pd,
    function(x) {
      sym_cond <- which(x$token == "SYMBOL" & x$text %in% object_names)
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

  effects <- lapply(object_names, return_code_for_effects, pd = calls_pd, occur = occurrence, cooccur = cooccurrence)
  names(effects) <- object_names

  list(
    occurrence = occurrence,
    cooccurrence = cooccurrence,
    effects = effects
  )
}

#' @title Get children calls within `getParseData()` object
#' @param pd `list` of data.frames of results of `utils::getParseData()` trimmed to unique `parsed_code` calls
#' @param parent parent id in `utils::getParseData()`
#' @return Row binded `utils::getParseData()` of all calls.
#' @keywords internal
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

#' @title Detects `"SYMBOL"` tokens for rbinded `getParseData()` structure
#' @param object `character` containing the name of the object
#' @param pd `list` of data.frames of results of `utils::getParseData()` trimmed to unique `parsed_code` calls
#' @keywords internal
detect_symbol <- function(object, pd = calls_pd) {
  unlist(
    lapply(
      pd,
      function(call) {
        any(call[call$token == "SYMBOL", "text"] == object)
      }
    )
  )
}

#' Return the lines of code needed to reproduce the object.
#' @return `numeric` vector indicating which lines of `parsed_code` calls are required to build the `object`
#'
#' @param object `character` with object name
#' @param pd `list` of data.frames of results of `utils::getParseData()` trimmed to unique `parsed_code` calls
#' @param occur result of `code_dependency()$occurrence`
#' @param cooccur result of `code_dependency()$cooccurrence`
#' @param parent `NULL` or `numeric` vector - in a recursive call, it is possible needed to drop parent object
#' indicator to omit dependency cycles
#'
#' @return A `numeric` vector with number of lines of input `pd` to be returned.
#'
#' @keywords internal
return_code <- function(object, pd = calls_pd, occur = occurrence, cooccur = cooccurrence, parent = NULL) {
  influences <-
    lapply(
      cooccur,
      function(x) {
        if (!is.null(x) && object %in% x[-1]) {
          TRUE
        } else if (!is.null(x) && object == x[1]) {
          FALSE
        }
      }
    )

  where_influences <- which(unlist(lapply(influences, isTRUE)))
  object_influencers <- which(unlist(lapply(influences, isFALSE)))

  object_influencers <- setdiff(object_influencers, parent)

  lines <- setdiff(occur[[object]], where_influences)

  if (length(object_influencers) == 0) {
    return(sort(unique(lines)))
  } else {
    for (idx in object_influencers) {
      # TRIM DOWN TO LINES ONLY NEEDED TO CREATE THE INITIAL OBJECT,
      # NOT TO ALL LINES OF THE INFLUENCER OBJECT.

      influencer_names <- cooccur[[idx]][-1]

      influencer_lines <-
        unlist(
          lapply(
            influencer_names,
            return_code,
            occur = lapply(occur, function(x) setdiff(x, idx:max(x))),
            cooccur = cooccur[1:idx],
            parent = where_influences
            # We need to skip parent_object so that we do not end up in a hole,
            # where e.g. in line 7 'A' gets influenced by 'B'
            # and in line 10 'B' gets influenced by 'A'.
          )
        )

      # If there is an @effect on the influencer.
      influencer_effects_lines <-
        unlist(
          lapply(
            influencer_names,
            return_code_for_effects,
            pd = pd,
            occur = occur,
            cooccur = cooccur # Do not trim to idx.
          )
        )
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
#' @param pd `list` of data.frames of results of `utils::getParseData()` trimmed to unique `parsed_code` calls
#' @param occur result of `code_dependency()$occurrence`
#' @param cooccur result of `code_dependency()$cooccurrence`
#'
#' @return A `numeric` vector with number of lines of input `pd` to be returned for effects.
#'
#' @keywords internal
return_code_for_effects <- function(object, pd = calls_pd, occur = occurrence, cooccur = cooccurrence) {
  symbol_effects_names <-
    unlist(
      lapply(
        pd,
        function(x) {
          com_cond <-
            x$token == "COMMENT" & grepl("@effect", x$text) & grepl(paste0("[\\s]*", object, "[\\s$]*"), x$text)

          # Make sure comment id is not the highest id in the item.
          # For calls like 'options(prompt = ">") # @effect ADLB',
          # 'options(prompt = ">")' is put in a one item
          # and '# @effect ADLB' is the first element of the next item.
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

  commented_calls <- vapply(pd,
    function(x) any(x$token == "COMMENT" & grepl("@effect", x$text)),
    FUN.VALUE = logical(1)
  )

  symbol_effects_lines <-
    unlist(
      lapply(
        symbol_effects_names,
        function(x) {
          code <- return_code(x, pd = pd, occur = occur, cooccur = cooccur)
          # QUESTION: SHOULD cooccur BE TRIMMED like it happens in return_code()?
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
            x$token == "COMMENT" & grepl("@effect", x$text) & grepl(paste0("[\\s]*", object, "[\\s$]*"), x$text)

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

#' Return the lines of code (with side-effects) needed to reproduce the object.
#' @return `character` vector of elements of `parsed_code` calls that were required to build the side-effects and
#' influencing objects having and impact on the `object`
#'
#' @param qenv `qenv` object
#' @param names `character` with object names
#' @keywords internal
get_code_dependency <- function(qenv, names) {
  code_dependency <- Reduce(bind_code_dependency, qenv@code_dependency)

  parsed_code <- parse(text = as.character(qenv@code))
  pd <- getParseData(parsed_code)
  calls_pd <- lapply(pd[pd$parent == 0, "id"], get_children, pd = pd)

  lines <- list()
  for(name in names) {
    object_lines <-
      return_code(
        name,
        pd = calls_pd,
        occur = code_dependency$occurrence,
        cooccur = code_dependency$cooccurrence
      )

    effects_lines <- code_dependency$effects[[name]]

    lines[[name]] <- c(object_lines, effects_lines)
  }

  object_lines_unique <- sort(unique(unlist(lines)))

  as.character(parsed_code)[object_lines_unique]
  # or
  # srcref <- attr(parsed_code, 'srcref')
  # unlist(lapply(srcref, as.character))[object_lines_unique]
}

#' @title Bind two `code_dependency()` lists
#' @param code1,code2 outputs of `code_dependency()`
#' @keywords internal
bind_code_dependency <- function(old_code_dep, new_code_dep) {
  # length(old_code_dep$cooccurrence) = lines of code in old_code
  new_code_dep$occurrence <- lapply(new_code_dep$occurrence, function(x) x + length(old_code_dep$cooccurrence))
  new_code_dep$effects <- lapply(
    new_code_dep$effects,
    function(x) {
      if (!is.null(x)) {
        x + length(old_code_dep$cooccurrence)
      }
    }
  )

  occurrence <- bind_lists(old_code_dep$occurrence, new_code_dep$occurrence)
  cooccurrence <- c(old_code_dep$cooccurrence, new_code_dep$cooccurrence)
  effects <- bind_lists(old_code_dep$effects, new_code_dep$effects)

  list(
    occurrence = occurrence,
    cooccurrence = cooccurrence,
    effects = effects
  )
}

bind_lists <- function(list1, list2) {
  if (length(list1) == 0) {
    return(list2)
  }

  list1_only <- setdiff(names(list1), names(list2))
  list2_only <- setdiff(names(list2), names(list1))
  both_lists <- intersect(names(list1), names(list2))

  c(list1[list1_only], Map(c, list1[both_lists], list2[both_lists]), list2[list2_only])
}
