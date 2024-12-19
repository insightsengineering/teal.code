#' Get code from `qenv`
#'
#' @description
#' Retrieves the code stored in the `qenv`.
#'
#' @param object (`qenv`)
#' @param deparse (`logical(1)`) flag specifying whether to return code as `character` or `expression`.
#' @param ... internal usage, please ignore.
#' @param names (`character`) `r lifecycle::badge("experimental")` vector of object names to return the code for.
#' For more details see the "Extracting dataset-specific code" section.
#'
#' @section Extracting dataset-specific code:
#'
#' `get_code(object, names)` limits the returned code to contain only those lines needed to _create_
#' the requested objects. The code stored in the `qenv` is analyzed statically to determine
#' which lines the objects of interest depend upon. The analysis works well when objects are created
#' with standard infix assignment operators (see `?assignOps`) but it can fail in some situations.
#'
#' Consider the following examples:
#'
#' _Case 1: Usual assignments._
#' ```r
#' q1 <-
#'   within(qenv(), {
#'     foo <- function(x) {
#'       x + 1
#'     }
#'     x <- 0
#'     y <- foo(x)
#'   })
#' get_code(q1, names = "y")
#' ```
#' `x` has no dependencies, so `get_code(data, names = "x")` will return only the second call.\cr
#' `y` depends on `x` and `foo`, so `get_code(data, names = "y")` will contain all three calls.
#'
#' _Case 2: Some objects are created by a function's side effects._
#' ```r
#' q2 <-
#'   within(qenv(){
#'     foo <- function() {
#'       x <<- x + 1
#'     }
#'     x <- 0
#'     foo()
#'     y <- x
#'   })
#' get_code(q2, names = "y")
#' ```
#' Here, `y` depends on `x` but `x` is modified by `foo` as a side effect (not by reassignment)
#' and so `get_code(data, names = "y")` will not return the `foo()` call.\cr
#' To overcome this limitation, code dependencies can be specified manually.
#' Lines where side effects occur can be flagged by adding "`# @linksto <object name>`" at the end.\cr
#' Note that `within` evaluates code passed to `expr` as is and comments are ignored.
#' In order to include comments in code one must use the `eval_code` function instead.
#'
#' ```r
#' q3 <-
#'   eval_code(qenv(), "
#'     foo <- function() {
#'       x <<- x + 1
#'     }
#'     x <- 0
#'     foo() # @linksto x
#'     y <- x
#'   ")
#' get_code(q3, names = "y")
#' ```
#' Now the `foo()` call will be properly included in the code required to recreate `y`.
#'
#' Note that two functions that create objects as side effects, `assign` and `data`, are handled automatically.
#'
#' Here are known cases where manual tagging is necessary:
#' - non-standard assignment operators, _e.g._ `%<>%`
#' - objects used as conditions in `if` statements: `if (<condition>)`
#' - objects used to iterate over in `for` loops: `for(i in <sequence>)`
#' - creating and evaluating language objects, _e.g._ `eval(<call>)`
#'
#' @return
#' The code used in the `qenv` in the form specified by `deparse`.
#'
#' @examples
#' # retrieve code
#' q <- within(qenv(), {
#'   a <- 1
#'   b <- 2
#' })
#' get_code(q)
#' get_code(q, deparse = FALSE)
#' get_code(q, names = "a")
#'
#' q <- qenv()
#' q <- eval_code(q, code = c("a <- 1", "b <- 2"))
#' get_code(q, names = "a")
#'
#' @aliases get_code,qenv-method
#' @aliases get_code,qenv.error-method
#'
#' @export
setGeneric("get_code", function(object, deparse = TRUE, names = NULL, ...) {
  dev_suppress(object)
  standardGeneric("get_code")
})

setMethod("get_code", signature = "qenv", function(object, deparse = TRUE, names = NULL, ...) {
  checkmate::assert_flag(deparse)
  checkmate::assert_character(names, min.len = 1L, null.ok = TRUE)

  # Normalize in case special it is backticked
  if (!is.null(names)) {
    names <- gsub("^`(.*)`$", "\\1", names)
  }

  code <- if (!is.null(names)) {
    get_code_dependency(object@code, names, ...)
  } else {
    object@code
  }

  if (deparse) {
    paste(unlist(code), collapse = "\n")
  } else {
    parse(text = paste(c("{", unlist(code), "}"), collapse = "\n"), keep.source = TRUE)
  }
})

setMethod("get_code", signature = "qenv.error", function(object, ...) {
  stop(
    errorCondition(
      sprintf(
        "%s\n\ntrace: \n %s\n",
        conditionMessage(object),
        paste(object$trace, collapse = "\n ")
      ),
      class = c("validation", "try-error", "simpleError")
    )
  )
})
