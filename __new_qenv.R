
#' qenv refactor prototype
#'
#' Simple to use environment with history tracking.
#'
#' @param data (`qenv`)
#' @param expr (`language`) simple or compound expression to evaluate in `data`
#' @param text (`character`) character vector of expressions to evaluate in `data`
#' @param ... (`pair-list`) `name:value` pairs to inject values into `expr`
#' @param x (`qenv`)
#'
#' @return
#' `qenv` returns a `qenv` object. `with` returns NULL invisibly.
#'
#' @describeIn qenv create `qenv` object
#' @export
qenv <- function(file) {
  ans <- new.env()
  attr(ans, "code") <- list()
  attr(ans, "errors") <- list()
  attr(ans, "warnings") <- list()
  attr(ans, "messages") <- list()
  class(ans) <- c("qenv", class(ans))
  ans
}


#' @describeIn qenv act in `qenv` object
#' @export
with.qenv <- function(data, expr, text, ...) {

  code <- .prepare_code(if (!missing(expr)) substitute(expr), if (!missing(text)) text)

  extras <- list(...)

  lapply(code, .eval_one, envir = data, enclos = parent.frame(), extras = extras)

  invisible(NULL)
}


#' @export
format.qenv <- function(x) {
  # opening message
  header <- paste(
    "`qenv` object (environment)",
    "  Use `with(qenv, { <code> })` to evaluate code in the qenv.",
    "  Use `get_code(qenv)` to access all code run in the qenv since instantiation.",
    "  Use `qenv$<variable>` or `qenv[[\"<variable>\"]]`to access variables.",
    sep = "\n"
  )

  # contents/bindings
  var_names <- ls(x)
  var_names_hidden <- setdiff(ls(x, all.names = TRUE), var_names)

  if (length(var_names) + length(var_names_hidden) > 0L) {
    var_classes <- sapply(var_names, function(vn) toString(class(get(vn, envir = x, inherits = FALSE))))
    var_classes_hidden <- sapply(var_names_hidden, function(vn) toString(class(get(vn, envir = x, inherits = FALSE))))


    longest_name <- max(nchar(c(var_names, var_names_hidden)))
    longest_class <- max(nchar(c(var_classes, var_classes_hidden)))

    contents <- if (is.finite(longest_name)) paste(
      sprintf(sprintf("  $ %%-0%is", longest_name), var_names),
      sprintf(sprintf("%%-0%is", longest_class), var_classes),
      sep = " : ", collapse = "\n")

    contents_hidden <- paste(
      sprintf(sprintf("  $ %%-0%is", longest_name), var_names_hidden),
      sprintf(sprintf("%%-0%is", longest_class), var_classes_hidden),
      sep = " : ", collapse = "\n")

    contents_all <- c(
      if (!identical(contents, "")) sprintf("bindings:\n%s", contents),
      if (!identical(contents_hidden, "")) sprintf("hidden bindings:\n%s", contents_hidden)
    )
  } else {
    contents_all <- "This qenv is empty."
  }

  # code
  code <- attr(x, "code")
  code <-
    if (identical(code, list())) {
      ""
    } else {
      paste(
        "  {",
        paste(sprintf("      %s", lapply(code, deparse)), collapse = "\n"),
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


#' @export
print.qenv <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}


#' @export
`[.qenv` <- function(x, ...) {
  stop("Use `qenv$<variable>` or `qenv[[\"<variable>\"]]`to access variables.")
}


#' @export
`$<-.qenv` <- function(x, name, value) {
  stop(
    "Direct assignment is forbidden as it cannot be tracked. ",
    "Use", sprintf("`with( <qenv>, { %s <- %s })`", name, deparse(value)), " instead."
  )
}


#' @describeIn qenv Returns list of function calls or a data.frame with code and the conditions it raised.
#' @export
get_code <- function(x, include_messages = FALSE) {
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


#' @describeIn qenv Returns list of condition messages (character strings).
#' @export
get_conditions <- function(x, condition = c("errors", "warnings", "messages", "all")) {
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


#' @examples
#'
#' q <- qenv()
#' # execute code
#' with(q, {
#'   i <- iris
#'   m <- mtcars
#' })
#' q
#' # error messages are stored
#' with(q, {
#'   subset(i, Species == species) # raises error and stops evaluation
#'   ms <- subset(m, cyl == 4) # not evaluated
#' })
#' q
#' # warnings and messages are also stored
#' with(q, {
#'   warning("this is a warning")
#' })
#' with(q, {
#'   message("this is a message")
#' })
#' q
#'
#' access variables and environment history
#' q$m
#' get_code(q)
#' get_conditions(q, "error")
#'
#' # injecting values into code
#' q <- qenv()
#' with(q, i <- iris)
#' with(q, print(dim(subset(i, Species == "virginica"))))
#' \dontrun{
#' with(q, print(dim(subset(i, Species == species)))) # fails
#' }
#' with(q, print(dim(subset(i, Species == species))), species = "versicolor")
#' species_external <- "versicolor"
#' with(q, print(dim(subset(i, Species == species))), species = species_external)


# internal; work in progress
object_info <- function(x) UseMethod("object_info")
object_info.data.frame <- function(x) sprintf("%d x %d", dim(x)[1], dim(x)[2])              # nolint
object_info.matrix <- function(x) sprintf("%s, %d x %d", typeof(x), dim(x)[1], dim(x)[2])   # nolint
object_info.factor <- function(x) sprintf("%d levels, [%d]", length(levels(x)), length(x))  # nolint
object_info.default <- function(x) sprintf("%s, [%d]", typeof(x), length(x))                # nolint


#' @keywords internal
# internal function to prepare expression(s) for evaluation
.prepare_code <- function(expr, text) {

  parent_call <- match.call(definition = sys.function(2), call = sys.call(2))

  if ((is.null(expr) && is.null(text)) || (!is.null(expr) && !is.null(text))) {
    stop("specify either \"expr\" or \"text\": ", deparse1(parent_call), call. = FALSE)
  }

  if (!is.null(expr) && is.character(expr)) {
    stop("character vector passed to \"expr\": ", deparse1(parent_call), call. = FALSE)
  }

  if (!is.null(expr) && !grepl("^\\{", deparse1(expr))) {
    expr <- call("{", expr)
  }

  if (!is.null(text) && length(text) == 1L && grepl("^\\{", text)) {
    text <- strsplit(text, split = "\n")[[1]]
    text <- trimws(text, whitespace = "[ \t\r\n\\{\\}]")
    text <- Filter(Negate(function(x) identical(x, "")), text)
  }

  code <-
    if (is.null(text)) {
      Filter(Negate(is.character), as.list(expr)[-1])
    } else if (is.null(expr)) {
      text
    }

  code
}


#' @keywords internal
# internal function to evaluate one expression
.eval_one <- function(expression, envir, enclos, extras) {
  on.exit(
    lapply(c("errors", "warnings", "messages"), function(c) {
      if (length(attr(envir, c)) < length(attr(envir, "code"))) {
        attr(envir, c) <- append(attr(envir, c), "")
      }
    })
  )

  expression <-
    if (is.character(expression)) {
      str2lang(expression)
    } else {
      do.call(substitute, list(expr = expression, env = extras))
    }

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
