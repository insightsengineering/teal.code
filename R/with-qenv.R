#' qenv refactor prototype
#'
#' Simple to use environment with history tracking.
#'
#' @details
#' Create a `quenv` object, which is an environment, and execute code inside.
#' Code can be supplied as expressions, literal character vectors, as well as name-bound character vectors.
#' External values can be injected into the code with the ellipsis.
#'
#' `quenv` creates `quenv` object.
#' `with` acts in `quenv` object.
#' `within` creates and modifies a (deep) copy of `quenv` object.
#' `get_code` returns list of function calls or a data.frame with code and the conditions it raised.
#' `get_conditions` returns list of condition messages (character strings).
#'
#' @param data,x (`quenv`)
#' @param expr (`language`) simple or compound expression to evaluate in `data`
#' @param text (`character`) character vector of expressions to evaluate in `data`
#' @param ... `name:value` pairs to inject values into `expr`
#'
#' @return
#' `quenv` returns a `quenv` object. `with` returns NULL invisibly. `within` returns a modified deep copy of `data`.
#'
#' @name quenv
#'
#' @examples
#'
#' q <- quenv()
#'
#' # execute code
#' with(q, {
#'   i <- iris
#'   m <- mtcars
#' })
#' q
#'
#' # supply code as strings
#' q <- quenv()
#' with(q, text = "c <- cars")
#' code_as_text <- "w <- warpbreaks"
#' with(q, text = code_as_text)
#'
#' # error messages are stored
#' try(
#'   with(q, {
#'     subset(i, Species == species) # raises error and stops evaluation
#'     ms <- subset(m, cyl == 4) # not evaluated
#'   })
#' )
#' q
#'
#' # warnings and messages are also stored
#' with(q, {
#'   warning("this is a warning")
#' })
#' with(q, {
#'   message("this is a message")
#' })
#' q
#'
#' # access variables and environment history
#' q$m
#' get_code(q)
#' get_conditions(q, "error")
#'
#' # inject values into code
#' q <- quenv()
#' with(q, i <- iris)
#' with(q, print(dim(subset(i, Species == "virginica"))))
#' try(
#'   with(q, print(dim(subset(i, Species == species)))) # fails
#' )
#' with(q, print(dim(subset(i, Species == species))), species = "versicolor")
#' species_external <- "versicolor"
#' with(q, print(dim(subset(i, Species == species))), species = species_external)
#'
#' # execute code in copy of `quenv` and return modified copy
#' q <- quenv()
#' with(q, i <- iris)
#' qq <- within(q, m <- mtcars)
#'

#' @rdname quenv
#' @export
quenv <- function() {
  ans <- new.env()
  attr(ans, "code") <- list()
  attr(ans, "errors") <- list()
  attr(ans, "warnings") <- list()
  attr(ans, "messages") <- list()
  class(ans) <- c("quenv", class(ans))
  ans
}


#' @rdname quenv
#' @export
with.quenv <- function(data, expr, text, ...) {
  code <- .prepare_code(if (!missing(expr)) substitute(expr), if (!missing(text)) text)
  extras <- list(...)
  lapply(code, .eval_one, envir = data, enclos = parent.frame(), extras = extras)
  invisible(NULL)
}


#' @rdname quenv
#' @export
within.quenv <- function(data, expr, text, ...) {
  # Force a return even if some evaluation fails.
  on.exit(return(data))
  data <- .clone_quenv(data)
  code <- .prepare_code(if (!missing(expr)) substitute(expr), if (!missing(text)) text)
  extras <- list(...)
  lapply(code, .eval_one, envir = data, enclos = parent.frame(), extras = extras)
}


#' @rdname quenv
#' @export
#' @keywords internal
format.quenv <- function(x) {
  # opening message
  header <- paste(
    "`quenv` object (environment)",
    "  Use `with(quenv, { <code> })` to evaluate code in the quenv.",
    "  Use `get_code(quenv)` to access all code run in the quenv since instantiation.",
    "  Use `quenv$<variable>` or `quenv[[\"<variable>\"]]`to access variables.",
    sep = "\n"
  )

  # contents/bindings # nolint
  var_names <- ls(x)
  var_names_hidden <- setdiff(ls(x, all.names = TRUE), var_names)

  if (length(var_names) + length(var_names_hidden) > 0L) {
    var_classes <- sapply(var_names, function(vn) toString(class(get(vn, envir = x, inherits = FALSE))))
    var_classes_hidden <- sapply(var_names_hidden, function(vn) toString(class(get(vn, envir = x, inherits = FALSE))))


    longest_name <- max(nchar(c(var_names, var_names_hidden)))
    longest_class <- max(nchar(c(var_classes, var_classes_hidden)))

    contents <- if (is.finite(longest_name)) {
      paste(
        sprintf(sprintf("  $ %%-0%is", longest_name), var_names),
        sprintf(sprintf("%%-0%is", longest_class), var_classes),
        vapply(var_names, function(v) .object_info(get(v, envir = x)), character(1)),
        sep = " : ", collapse = "\n"
      )
    }

    contents_hidden <- paste(
      sprintf(sprintf("  $ %%-0%is", longest_name), var_names_hidden),
      sprintf(sprintf("%%-0%is", longest_class), var_classes_hidden),
      vapply(var_names_hidden, function(v) .object_info(get(v, envir = x)), character(1)),
      sep = " : ", collapse = "\n"
    )

    contents_all <- c(
      if (!identical(contents, "")) sprintf("bindings:\n%s", contents),
      if (!identical(contents_hidden, "")) sprintf("hidden bindings:\n%s", contents_hidden)
    )
  } else {
    contents_all <- "This quenv is empty."
  }

  # code
  code <- attr(x, "code")
  code <-
    if (identical(code, list())) {
      ""
    } else {
      expressions <- vapply(code, function(x) paste(sprintf("      %s", deparse(x)), collapse = "\n"), character(1L))
      paste(
        "  {",
        paste(expressions, collapse = "\n"),
        "  }",
        sep = "\n"
      )
    }
  code <- if (!identical(code, "")) sprintf("code:\n%s", code)

  # conditions
  conditions <- lapply(get_conditions(x, "all"), sprintf, fmt = "  %s")
  conditions <- unlist(
    mapply(
      function(value, name) sprintf("%s:\n%s", name, paste(value, collapse = "\n")),
      value = conditions, name = names(conditions)
    )
  )

  # closing message
  footer <- sprintf("parent: %s", format(parent.env(x)))

  c(
    header,
    "",
    contents_all,
    "",
    code,
    "",
    conditions,
    "",
    footer
  )
}


#' @rdname quenv
#' @export
#' @keywords internal
print.quenv <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}


#' @export
#' @keywords internal
`[.quenv` <- function(x, ...) { # nolint
  stop("Use `quenv$<variable>` or `quenv[[\"<variable>\"]]`to access variables.")
}


#' @export
#' @keywords internal
`$<-.quenv` <- function(x, name, value) { # nolint
  stop(
    "Direct assignment is forbidden as it cannot be tracked. ",
    "Use `with( <quenv>, { <name> <- <value> })` instead."
  )
}


#' @export
#' @keywords internal
`[[<-.quenv` <- function(x, name, value) { # nolint
  stop(
    "Direct assignment is forbidden as it cannot be tracked. ",
    "Use `with( <quenv>, { <name> <- <value> })` instead."
  )
}


#' @rdname quenv
#' @export
#' @keywords internal
get_code_quenv <- function(x, include_messages = FALSE) {
  checkmate::assert_class(x, "quenv")
  if (include_messages) {
    collected <- list(
      code = lapply(attr(x, "code"), deparse1),
      error = attr(x, "errors"),
      warning = attr(x, "warnings"),
      message = attr(x, "messages")
    )
    as.data.frame(lapply(collected, unlist))
  } else {
    attr(x, "code")
  }
}


#' @rdname quenv
#' @export
#' @keywords internal
get_conditions <- function(x, condition = c("errors", "warnings", "messages", "all")) {
  checkmate::assert_class(x, "quenv")
  condition <- match.arg(condition)

  if (condition == "all") {
    Filter(
      function(xxx) !identical(xxx, list()),
      lapply(
        attributes(x)[c("errors", "warnings", "messages")],
        function(xx) Filter(function(x) !identical(x, ""), xx)
      )
    )
  } else {
    Filter(function(x) !identical(x, ""), attr(x, condition))
  }
}


#' @keywords internal
#  prepare expression(s) for evaluation
.prepare_code <- function(expr, text) {
  # This function cannot handle missing arguments, so the caller passes if statements that return NULL if FALSE.

  # Get parent call to use in error messages.
  parent_call <- deparse1(match.call(definition = sys.function(2), call = sys.call(2)))

  if ((is.null(expr) && is.null(text)) || (!is.null(expr) && !is.null(text))) {
    stop("specify either \"expr\" or \"text\": ", parent_call, call. = FALSE)
  }

  if (!is.null(expr) && is.character(expr)) {
    stop(
      "character vector passed to \"expr\": ", parent_call, "\n  use the \"text\" argument instead",
      call. = FALSE
    )
  }

  # Add braces to expressions. Necessary for proper storage of some expressions (e.g. rm(x)).
  if (!is.null(expr) && !grepl("^\\{", deparse1(expr))) {
    expr <- call("{", expr)
  }

  code <-
    if (is.null(text)) {
      # Drop strings from compound expressions.
      Filter(Negate(is.character), as.list(expr)[-1])
    } else if (is.null(expr)) {
      expr <- as.list(str2expression(text))
      unpack <- function(x) {
        if (identical(x[[1L]], as.symbol("{"))) {
          as.character(x[-1L])
        } else {
          deparse1(x)
        }
      }
      unlist(lapply(expr, unpack))
    }
  code
}


#' @keywords internal
# evaluate one expression, log any conditions raised
.eval_one <- function(expression, envir, enclos, extras) {
  # Add empty string if no condition raised during evaluation.
  on.exit(
    lapply(c("errors", "warnings", "messages"), function(c) {
      if (length(attr(envir, c)) < length(attr(envir, "code"))) {
        attr(envir, c) <- append(attr(envir, c), "")
      }
    })
  )

  if (is.character(expression)) {
    expression <- str2lang(expression)
  }
  expression <- do.call(substitute, list(expr = expression, env = extras))

  attr(envir, "code") <- append(attr(envir, "code"), expression)
  tryCatch(
    eval(expression, envir = envir, enclos = enclos),
    message = function(m) attr(envir, "messages") <- append(attr(envir, "messages"), trimws(m$message)),
    warning = function(w) attr(envir, "warnings") <- append(attr(envir, "warnings"), w$message),
    error = function(e) {
      attr(envir, "errors") <- append(attr(envir, "errors"), e$message)
      stop(sprintf("Evaluation failed: %s", deparse1(expression)), call. = FALSE)
    }
  )
}


#' @keywords internal
# deep copy a `quenv`
.clone_quenv <- function(x) {
  if (!inherits(x, "quenv")) stop("\"x\" must be a quenv object")
  ans <- list2env(mget(ls(envir = x, all.names = TRUE, sorted = FALSE), envir = x), parent = parent.env(x))
  attributes(ans) <- attributes(x)
  ans
}



# helper for `format.quenv`
# briefly summarize object
#' @export
#' @keywords internal
.object_info <- function(x) { # nolint
  # nolint
  UseMethod(".object_info")
}
#' @export
#' @keywords internal
.object_info.data.frame <- function(x) { # nolint
  # nolint
  sprintf("%d x %d", dim(x)[1], dim(x)[2])
}
#' @export
#' @keywords internal
.object_info.matrix <- function(x) { # nolint
  # nolint
  sprintf("%s, %d x %d", typeof(x), dim(x)[1], dim(x)[2])
}
#' @export
#' @keywords internal
.object_info.factor <- function(x) { # nolint
  # nolint
  sprintf("%d levels, [%d]", length(levels(x)), length(x))
}
#' @export
#' @keywords internal
.object_info.character <- function(x) { # nolint
  # nolint
  sprintf("%d item(s), %d value(s)", length(x), length(unique(x)))
}
#' @export
#' @keywords internal
.object_info.numeric <- function(x) { # nolint
  # nolint
  sprintf("%d item(s)", length(x))
}
#' @export
#' @keywords internal
.object_info.default <- function(x) { # nolint
  # nolint
  ""
}
