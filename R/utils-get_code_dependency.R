# get_code_dependency ----

#' Get code dependency of an object
#'
#' Extract subset of code required to reproduce specific object(s), including code producing side-effects.
#'
#' Given a character vector with code, this function will extract the part of the code responsible for creating
#' the variables specified by `names`.
#' This includes the final call that creates the variable(s) in question as well as all _parent calls_,
#' _i.e._ calls that create variables used in the final call and their parents, etc.
#' Also included are calls that create side-effects like establishing connections.
#'
#' It is assumed that object dependency is established by using three assignment operators: `<-`, `=`, and `->` .
#' Other assignment methods (`assign`, `<<-`) or non-standard-evaluation methods are not supported.
#'
#' Side-effects are not detected automatically and must be marked in the code.
#' Add `# @linksto object` at the end of a line where a side-effect occurs to specify that this line is required
#' to reproduce a variable called `object`.
#'
#' @param code `character` with the code.
#' @param names `character` vector of object names.
#' @param check_code_names `logical(1)` flag specifying if a warning for non-existing names should be displayed.
#'
#' @return Character vector, a subset of `code`.
#' Note that subsetting is actually done on the calls `code`, not necessarily on the elements of the vector.
#'
#' @keywords internal
get_code_dependency <- function(code, names, check_code_names = TRUE) {
  checkmate::assert_list(code, "character")
  checkmate::assert_character(names, any.missing = FALSE)

  graph <- lapply(code, attr, "dependency")

  if (check_code_names) {
    symbols <- unlist(lapply(graph, function(call) {
      ind <- match("<-", call, nomatch = length(call) + 1L)
      call[seq_len(ind - 1L)]
    }))

    if (!all(names %in% unique(symbols))) {
      warning("Object(s) not found in code: ", toString(setdiff(names, symbols)), ".", call. = FALSE)
    }
  }

  if (length(code) == 0) {
    return(code)
  }

  ind <- unlist(lapply(names, function(x) graph_parser(x, graph)))

  lib_ind <- detect_libraries(graph)

  code_ids <- sort(unique(c(lib_ind, ind)))
  code[code_ids]
}

#' Locate function call token
#'
#' Determine which row of parsed data is specific `SYMBOL_FUNCTION_CALL` token.
#'
#' Useful for determining occurrence of `assign` or `data` functions in an input call.
#'
#' @param call_pd `data.frame` as returned by `extract_calls()`
#' @param text `character(1)` to look for in `text` column of `call_pd`
#'
#' @return
#' Single integer specifying row in `call_pd` where `token` is `SYMBOL_FUNCTION_CALL` and `text` is `text`.
#' 0 if not found.
#'
#' @keywords internal
#' @noRd
find_call <- function(call_pd, text) {
  checkmate::check_data_frame(call_pd)
  checkmate::check_names(call_pd, must.include = c("token", "text"))
  checkmate::check_string(text)

  ans <- which(call_pd$token == "SYMBOL_FUNCTION_CALL" & call_pd$text == text)
  if (length(ans)) {
    ans
  } else {
    0L
  }
}

#' Split the result of `utils::getParseData()` into separate calls
#'
#' @param pd (`data.frame`) A result of `utils::getParseData()`.
#'
#' @return
#' A `list` of `data.frame`s.
#' Each element is a subset of `pd` corresponding to one call in the original code from which `pd` was obtained.
#' Only four columns (`"token"`, `"text"`, `"id"`, `"parent"`) are kept, the rest is discarded.
#'
#' @keywords internal
#' @noRd
extract_calls <- function(pd) {
  calls <- lapply(
    pd[pd$parent == 0 & (pd$token != "COMMENT" | grepl("@linksto", pd$text, fixed = TRUE)), "id"],
    function(parent) {
      rbind(
        pd[pd$id == parent, ],
        get_children(pd = pd, parent = parent)
      )
    }
  )
  calls <- Filter(function(call) !(nrow(call) == 1 && call$token == "';'"), calls)
  calls <- Filter(Negate(is.null), calls)
  calls <- fix_shifted_comments(calls)
  calls <- remove_custom_assign(calls, c(":="))
  fix_arrows(calls)
}

#' @keywords internal
#' @noRd
get_children <- function(pd, parent) {
  idx_children <- abs(pd$parent) == parent
  children <- pd[idx_children, ]
  if (nrow(children) == 0) {
    return(NULL)
  }

  if (parent > 0) {
    do.call(rbind, c(list(children), lapply(children$id, get_children, pd = pd)))
  }
}

#' Fixes edge case of comments being shifted to the next call.
#' @keywords internal
#' @noRd
fix_shifted_comments <- function(calls) {
  # If the first or the second token is a @linksto COMMENT,
  #  then it belongs to the previous call.
  if (length(calls) >= 2) {
    for (i in 2:length(calls)) {
      comment_idx <- grep("@linksto", calls[[i]][, "text"])
      if (isTRUE(comment_idx[1] <= 2)) {
        calls[[i - 1]] <- rbind(
          calls[[i - 1]],
          calls[[i]][comment_idx[1], ]
        )
        calls[[i]] <- calls[[i]][-comment_idx[1], ]
      }
    }
  }
  Filter(nrow, calls)
}

#' Fixes edge case of custom assignments operator being treated as assignment.
#'
#' @param exclude (`character`) custom assignment operators to be excluded
#' @keywords internal
#' @noRd
remove_custom_assign <- function(calls, exclude = NULL) {
  checkmate::assert_list(calls)
  checkmate::assert_character(exclude, null.ok = TRUE)
  lapply(calls, function(call) {
    if (!is.null(exclude)) {
      call[!(call$token == "LEFT_ASSIGN" & call$text %in% exclude), ]
    } else {
      call
    }
  })
}

#' Fixes edge case of `<-` assignment operator being called as function,
#' which is \code{`<-`(y,x)} instead of traditional `y <- x`.
#' @keywords internal
#' @noRd
fix_arrows <- function(calls) {
  checkmate::assert_list(calls)
  lapply(calls, function(call) {
    sym_fun <- call$token == "SYMBOL_FUNCTION_CALL"
    call[sym_fun, ] <- sub_arrows(call[sym_fun, ])
    call
  })
}

#' Execution of assignment operator substitutions for a call.
#' @keywords internal
#' @noRd
sub_arrows <- function(call) {
  checkmate::assert_data_frame(call)
  map <- data.frame(
    row.names = c("<-", "<<-", "="),
    token = rep("LEFT_ASSIGN", 3),
    text = rep("<-", 3)
  )
  sub_ids <- call$text %in% rownames(map)
  call[sub_ids, c("token", "text")] <- map[call$text[sub_ids], ]
  call
}

# code_graph ----

#' Extract object occurrence
#'
#' Extracts objects occurrence within calls passed by `pd`.
#' Also detects which objects depend on which within a call.
#'
#' @param pd `data.frame`;
#'  one of the results of `utils::getParseData()` split into subsets representing individual calls;
#'  created by `extract_calls()` function
#'
#' @return
#' A character vector listing names of objects that depend on this call
#' and names of objects that this call depends on.
#' Dependencies are listed after the `"<-"` string, e.g. `c("a", "<-", "b", "c")` means that in this call object `a`
#' depends on objects `b` and `c`.
#' If a call is tagged with `@linksto a`, then object `a` is understood to depend on that call.
#'
#' @keywords internal
#' @noRd
extract_occurrence <- function(pd) {
  is_in_function <- function(x) {
    # If an object is a function parameter,
    # then in calls_pd there is a `SYMBOL_FORMALS` entry for that object.
    function_id <- x[x$token == "FUNCTION", "parent"]
    if (length(function_id)) {
      x$id %in% get_children(x, function_id[1])$id
    } else {
      rep(FALSE, nrow(x))
    }
  }
  in_parenthesis <- function(x) {
    if (any(x$token %in% c("LBB", "'['"))) {
      id_start <- min(x$id[x$token %in% c("LBB", "'['")])
      id_end <- min(x$id[x$token == "']'"])
      x$text[x$token == "SYMBOL" & x$id > id_start & x$id < id_end]
    }
  }

  # Handle data(object)/data("object")/data(object, envir = ) independently.
  data_call <- find_call(pd, "data")
  if (data_call) {
    sym <- pd[data_call + 1, "text"]
    return(c(gsub("^['\"]|['\"]$", "", sym), "<-"))
  }
  # Handle assign(x = ).
  assign_call <- find_call(pd, "assign")
  if (assign_call) {
    # Check if parameters were named.
    # "','" is for unnamed parameters, where "SYMBOL_SUB" is for named.
    # "EQ_SUB" is for `=` appearing after the name of the named parameter.
    if (any(pd$token == "SYMBOL_SUB")) {
      params <- pd[pd$token %in% c("SYMBOL_SUB", "','", "EQ_SUB"), "text"]
      # Remove sequence of "=", ",".
      if (length(params > 1)) {
        remove <- integer(0)
        for (i in 2:length(params)) {
          if (params[i - 1] == "=" && params[i] == ",") {
            remove <- c(remove, i - 1, i)
          }
        }
        if (length(remove)) params <- params[-remove]
      }
      pos <- match("x", setdiff(params, ","), nomatch = match(",", params, nomatch = 0))
      if (!pos) {
        return(character(0L))
      }
      # pos is indicator of the place of 'x'
      # 1. All parameters are named, but none is 'x' - return(character(0L))
      # 2. Some parameters are named, 'x' is in named parameters: match("x", setdiff(params, ","))
      # - check "x" in params being just a vector of named parameters.
      # 3. Some parameters are named, 'x' is not in named parameters
      # - check first appearance of "," (unnamed parameter) in vector parameters.
    } else {
      # Object is the first entry after 'assign'.
      pos <- 1
    }
    sym <- pd[assign_call + pos, "text"]
    return(c(gsub("^['\"]|['\"]$", "", sym), "<-"))
  }

  # What occurs in a function body is not tracked.
  x <- pd[!is_in_function(pd), ]
  sym_cond <- which(x$token %in% c("SPECIAL", "SYMBOL", "SYMBOL_FUNCTION_CALL"))
  sym_fc_cond <- which(x$token == "SYMBOL_FUNCTION_CALL")

  if (length(sym_cond) == 0) {
    return(character(0L))
  }
  # Watch out for SYMBOLS after $ and @. For x$a x@a: x is object, a is not.
  # For x$a, a's ID is $'s ID-2 so we need to remove all IDs that have ID = $ID - 2.
  dollar_ids <- x[x$token %in% c("'$'", "'@'"), "id"]
  if (length(dollar_ids)) {
    object_ids <- x[sym_cond, "id"]
    after_dollar <- object_ids[(object_ids - 2) %in% dollar_ids]
    sym_cond <- setdiff(sym_cond, which(x$id %in% after_dollar))
  }

  assign_cond <- grep("ASSIGN", x$token)
  if (!length(assign_cond)) {
    return(c("<-", unique(x[sym_cond, "text"])))
  }

  # For cases like 'eval(expression(c <- b + 2))' removes 'eval(expression('.
  sym_cond <- sym_cond[!(sym_cond < min(assign_cond) & sym_cond %in% sym_fc_cond)]

  # If there was an assignment operation detect direction of it.
  if (unique(x$text[assign_cond]) == "->") { # What if there are 2 assignments: e.g. a <- b -> c.
    sym_cond <- rev(sym_cond)
  }

  after <- match(min(x$id[assign_cond]), sort(x$id[c(min(assign_cond), sym_cond)])) - 1
  ans <- append(x[sym_cond, "text"], "<-", after = max(1, after))
  ans <- move_functions_after_arrow(ans, unique(x[sym_fc_cond, "text"]))
  roll <- in_parenthesis(pd)
  if (length(roll)) {
    c(setdiff(ans, roll), roll)
  } else {
    ans
  }
}

#' Moves function names to the right side of dependency graph
#'
#' Changes status of the function call from dependent to dependency if occurs in the lhs.
#' Technically, it means to move function names after the dependency operator.
#' For example, for `attributes(a) <- b` the dependency graph should look like `c("a", "<-", "b", "attributes")`.
#'
#' @param ans `character` vector of object names in dependency graph.
#' @param functions `character` vector of function names.
#'
#' @return
#' A character vector.
#' @keywords internal
#' @noRd
move_functions_after_arrow <- function(ans, functions) {
  arrow_pos <- which(ans == "<-")
  if (length(arrow_pos) == 0) {
    return(ans)
  }
  before_arrow <- setdiff(ans[1:arrow_pos], functions)
  after_arrow <- ans[(arrow_pos + 1):length(ans)]
  c(before_arrow, unique(c(intersect(ans[1:arrow_pos], functions), after_arrow)))
}

#' Extract side effects
#'
#' Extracts all object names from the code that are marked with `@linksto` tag.
#'
#' The code may contain functions calls that create side effects, e.g. modify the environment.
#' Static code analysis may be insufficient to determine which objects are created or modified by such a function call.
#' The `@linksto` comment tag is introduced to mark a call as having a (side) effect on one or more objects.
#' With this tag a complete object dependency structure can be established.
#' Read more about side effects and the usage of `@linksto` tag in [`get_code_dependencies()`] function.
#'
#' @param pd `data.frame`;
#'  one of the results of `utils::getParseData()` split into subsets representing individual calls;
#'  created by `extract_calls()` function
#'
#' @return
#' A character vector of names of objects
#' depending a call tagged with `@linksto` in a corresponding element of `pd`.
#'
#' @keywords internal
#' @noRd
extract_side_effects <- function(pd) {
  linksto <- grep("@linksto", pd[pd$token == "COMMENT", "text"], value = TRUE)
  unlist(strsplit(sub("\\s*#.*@linksto\\s+", "", linksto), "\\s+"))
}

#' @param parsed_code results of `parse(text = code, keep.source = TRUE` (parsed text)
#' @keywords internal
#' @noRd
extract_dependency <- function(parsed_code) {
  full_pd <- normalize_pd(utils::getParseData(parsed_code))
  reordered_full_pd <- extract_calls(full_pd)

  # Early return on empty code
  if (length(reordered_full_pd) == 0L) {
    return(NULL)
  }

  if (length(parsed_code) == 0L) {
    return(extract_side_effects(reordered_full_pd[[1]]))
  }
  expr_ix <- lapply(parsed_code[[1]], class) == "{"

  # Build queue of expressions to parse individually
  queue <- list()
  parsed_code_list <- if (all(!expr_ix)) {
    list(parsed_code)
  } else {
    queue <- as.list(parsed_code[[1]][expr_ix])
    new_list <- parsed_code[[1]]
    new_list[expr_ix] <- NULL
    list(parse(text = as.expression(new_list), keep.source = TRUE))
  }

  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]
    if (identical(current[[1L]], as.name("{"))) {
      queue <- append(queue, as.list(current)[-1L])
    } else {
      parsed_code_list[[length(parsed_code_list) + 1]] <- parse(text = as.expression(current), keep.source = TRUE)
    }
  }

  parsed_occurences <- lapply(
    parsed_code_list,
    function(parsed_code) {
      pd <- normalize_pd(utils::getParseData(parsed_code))
      reordered_pd <- extract_calls(pd)
      if (length(reordered_pd) > 0) {
        # extract_calls is needed to reorder the pd so that assignment operator comes before symbol names
        # extract_calls is needed also to substitute assignment operators into specific format with fix_arrows
        # extract_calls is needed to omit empty calls that contain only one token `"';'"`
        # This cleaning is needed as extract_occurrence assumes arrows are fixed, and order is different
        # than in original pd
        extract_occurrence(reordered_pd[[1]])
      }
    }
  )

  # Merge results together
  result <- Reduce(
    function(u, v) {
      ix <- if ("<-" %in% v) min(which(v == "<-")) else 0
      u$left_side <- c(u$left_side, v[seq_len(max(0, ix - 1))])
      u$right_side <- c(
        u$right_side,
        if (ix == length(v)) character(0L) else v[seq(ix + 1, max(ix + 1, length(v)))]
      )
      u
    },
    init = list(left_side = character(0L), right_side = character(0L)),
    x = parsed_occurences
  )

  c(extract_side_effects(reordered_full_pd[[1]]), result$left_side, "<-", result$right_side)
}

# graph_parser ----

#' Return the indices of calls needed to reproduce an object
#'
#' @param x The name of the object to return code for.
#' @param graph A result of `code_graph()`.
#'
#' @return
#' Integer vector of indices that can be applied to `graph` to obtain all calls required to reproduce object `x`.
#'
#' @keywords internal
#' @noRd
graph_parser <- function(x, graph) {
  # x occurrences (lhs)
  occurrence <- vapply(
    graph, function(call) {
      ind <- match("<-", call, nomatch = length(call) + 1L)
      x %in% call[seq_len(ind - 1L)]
    },
    logical(1)
  )

  # x-dependent objects (rhs)
  dependencies <- lapply(graph[occurrence], function(call) {
    ind <- match("<-", call, nomatch = 0L)
    call[(ind + 1L):length(call)]
  })
  dependencies <- setdiff(unlist(dependencies), x)

  dependency_occurrences <- lapply(dependencies, function(dependency) {
    # track down dependencies and where they occur on the lhs in previous calls
    last_x_occurrence <- max(which(occurrence))
    reduced_graph <- utils::head(graph[seq_len(last_x_occurrence)], -1)
    c(graph_parser(dependency, reduced_graph), last_x_occurrence)
  })

  sort(unique(c(which(occurrence), unlist(dependency_occurrences))))
}


# default_side_effects --------------------------------------------------------------------------------------------

#' Detect library calls
#'
#' Detects `library()` and `require()` function calls.
#'
#' @param `graph` the dependency graph, result of `lapply(code, attr, "dependency")`
#'
#' @return
#' Integer vector of indices that can be applied to `graph` to obtain all calls containing
#' `library()` or `require()` calls that are always returned for reproducibility.
#'
#' @keywords internal
#' @noRd
detect_libraries <- function(graph) {
  defaults <- c("library", "require")

  which(
    unlist(
      lapply(
        graph, function(x) {
          any(grepl(pattern = paste(defaults, collapse = "|"), x = x))
        }
      )
    )
  )
}


# utils -----------------------------------------------------------------------------------------------------------


#' Normalize parsed data removing backticks from symbols
#'
#' @param pd `data.frame` resulting from `utils::getParseData()` call.
#'
#' @return `data.frame` with backticks removed from `text` column for `SYMBOL` tokens.
#'
#' @keywords internal
#' @noRd
normalize_pd <- function(pd) {
  # Remove backticks from SYMBOL tokens
  symbol_index <- grepl("^SYMBOL.*$", pd$token)
  pd[symbol_index, "text"] <- gsub("^`(.*)`$", "\\1", pd[symbol_index, "text"])

  pd
}


# split_code ------------------------------------------------------------------------------------------------------


#' Get line/column in the source where the calls end
#'
#'
#' @param code `character(1)`
#'
#' @return `matrix` with `colnames = c("line", "col")`
#'
#' @keywords internal
#' @noRd
get_call_breaks <- function(code) {
  parsed_code <- parse(text = code, keep.source = TRUE)
  pd <- utils::getParseData(parsed_code)
  pd <- normalize_pd(pd)
  pd <- pd[pd$token != "';'", ]
  call_breaks <- t(sapply(
    extract_calls(pd),
    function(x) {
      matrix(c(max(x$line2), max(x$col2[x$line2 == max(x$line2)])))
    }
  ))
  call_breaks <- call_breaks[-nrow(call_breaks), , drop = FALSE] # breaks in between needed only
  if (nrow(call_breaks) == 0L) {
    call_breaks <- matrix(numeric(0), ncol = 2)
  }
  colnames(call_breaks) <- c("line", "col")
  call_breaks
}

#' Split code by calls
#'
#' @param code `character` with the code.
#'
#' @return list of `character`s of the length equal to the number of calls in `code`.
#'
#' @keywords internal
#' @noRd
split_code <- function(code) {
  call_breaks <- get_call_breaks(code)
  if (nrow(call_breaks) == 0) {
    return(code)
  }
  call_breaks <- call_breaks[order(call_breaks[, "line"], call_breaks[, "col"]), , drop = FALSE]
  code_split <- strsplit(code, split = "\n", fixed = TRUE)[[1]]
  char_count_lines <- c(0, cumsum(sapply(code_split, nchar, USE.NAMES = FALSE) + 1), -1)[seq_along(code_split)]

  idx_start <- c(
    0, # first call starts in the beginning of src
    char_count_lines[call_breaks[, "line"]] + call_breaks[, "col"] + 1
  )
  idx_end <- c(
    char_count_lines[call_breaks[, "line"]] + call_breaks[, "col"],
    nchar(code) # last call end in the end of src
  )
  new_code <- substring(code, idx_start, idx_end)

  # line split happens before call terminator (it could be `;` or `\n`) and the terminator goes to the next line
  # we need to move remove leading and add \n instead when combining calls
  c(new_code[1], gsub("^[\t ]*(\n|;)", "", new_code[-1]))
}
