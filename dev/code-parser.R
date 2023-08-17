library(dplyr)
# we do not support
# - assign,
# - <<-
# - two appeareance of <- <- in one line


# for a given object, we return code lines of
# - where it was used                                 [Do we need that?]
# - where it was created
# - objects creation, that influence this object
# - side-effects, that influence this object [marked with # @effect tag]

code = '
  arm_mapping <- list(
    "A: Drug X" = "150mg QD",
    "B: Placebo" = "Placebo",
    "C: Combination" = "Combination"
  )
  color_manual <- c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C")
  # assign LOQ flag symbols: circles for "N" and triangles for "Y", squares for "NA"
  shape_manual <- c("N" = 1, "Y" = 2, "NA" = 0)
  ADSL <- goshawk::rADSL
  goshawk::rADLB-> ADLB
  iris2 <- iris # @effect ADLB ADSL
  var_labels <- lapply(ADLB, function(x) attributes(x)$label)
  iris3 <- iris
  ADLB <- ADLB %>%
    dplyr::mutate(AVISITCD = dplyr::case_when(
      AVISIT == "SCREENING" ~ "SCR",
      AVISIT == "BASELINE" ~ "BL",
      grepl("WEEK", AVISIT) ~
        paste(
          "W",
          trimws(
            substr(
              AVISIT,
              start = 6,
              stop = stringr::str_locate(AVISIT, "DAY") - 1
            )
          )
        ),
      TRUE ~ NA_character_
    )) %>%
    dplyr::mutate(AVISITCDN = dplyr::case_when(
      AVISITCD == "SCR" ~ -2,
      AVISITCD == "BL" ~ 0,
      grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
      TRUE ~ NA_real_
    )) %>%
    # use ARMCD values to order treatment in visualization legend
    dplyr::mutate(TRTORD = ifelse(grepl("C", ARMCD), 1,
                                  ifelse(grepl("B", ARMCD), 2,
                                         ifelse(grepl("A", ARMCD), 3, NA)
                                  )
    )) %>%
    dplyr::mutate(ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))])) %>%
    dplyr::mutate(ARM = factor(ARM) %>%
                    reorder(TRTORD)) %>%
    dplyr::mutate(
      ANRHI = dplyr::case_when(
        PARAMCD == "ALT" ~ 60,
        PARAMCD == "CRP" ~ 70,
        PARAMCD == "IGA" ~ 80,
        TRUE ~ NA_real_
      ),
      ANRLO = dplyr::case_when(
        PARAMCD == "ALT" ~ 20,
        PARAMCD == "CRP" ~ 30,
        PARAMCD == "IGA" ~ 40,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::rowwise() %>%
    dplyr::group_by(PARAMCD) %>%
    dplyr::mutate(LBSTRESC = ifelse(
      USUBJID %in% sample(USUBJID, 1, replace = TRUE),
      paste("<", round(runif(1, min = 25, max = 30))), LBSTRESC
    )) %>%
    dplyr::mutate(LBSTRESC = ifelse(
      USUBJID %in% sample(USUBJID, 1, replace = TRUE),
      paste(">", round(runif(1, min = 70, max = 75))), LBSTRESC
    )) %>%
    ungroup()
  attr(ADLB[["ARM"]], "label") <- var_labels[["ARM"]]
  attr(ADLB[["ANRHI"]], "label") <- "Analysis Normal Range Upper Limit"
  attr(ADLB[["ANRLO"]], "label") <- "Analysis Normal Range Lower Limit"
  mtcars # @effect ADLB
  options(prompt = ">") # @effect ADLB

  # add LLOQ and ULOQ variables
  ADLB_LOQS<-goshawk:::h_identify_loq_values(ADLB)
  goshawk:::h_identify_loq_values(ADLB)->ADLB_LOQS
  ADLB = dplyr::left_join(ADLB, ADLB_LOQS, by = "PARAM")
  iris6 <- list(ADLB, ADLB_LOQS, ADSL)
  iris5 <- iris
'

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
# First object in an element of the list is the dependent object, the others are influencers.

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
            parent = where_influences
            # We need to skip parent_object so that we do not end up in a hole,
            # where e.g. in line 7 'A' gets influenced by 'B'
            # and in line 10 'B' gets influenced by 'A'.
          )
        )

      lines <- c(lines, influencer_lines)
    }
    sort(unique(lines))
  }

}

#srcref[return_code('ADLB')]

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

#srcref[return_code_for_effects('ADLB')]

object_lines <- function(object) {
  sort(unique(c(return_code(object), return_code_for_effects(object))))
}
srcref[object_lines('ADLB')]
lapply(object_names, object_lines)
objects_code <-
  lapply(object_names, function(object) unlist(lapply(srcref[object_lines(object)], as.character)))

names(objects_code) <- object_names
