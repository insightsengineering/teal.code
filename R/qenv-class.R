#' Reproducible class with environment and code
#'
#' Reproducible class with environment and code.
#' @name qenv-class
#' @rdname qenv-class
#' @slot .xData (`environment`) environment with content was generated by the evaluation
#' @slot code (`named list` of `character`) representing code necessary to reproduce the environment.
#' Read more in Code section.
#'  of the `code` slot.
#'
#' @section Code:
#'
#' Each code element is a character representing one call. Each element is named with the random
#' identifier to make sure uniqueness when joining. Each element has possible attributes:
#' - `warnings` (`character`) the warnings output when evaluating the code element.
#' - `messages` (`character`) the messages output when evaluating the code element.
#' - `dependency` (`character`) names of objects that appear in this call and gets affected by this call,
#' separated by `<-` (objects on LHS of `<-` are affected by this line, and objects on RHS are affecting this line).
#'
#' @keywords internal
#' @exportClass qenv
setClass(
  "qenv",
  slots = c(code = "list"),
  contains = "environment"
)

#' It initializes the `qenv` class
#' @noRd
setMethod(
  "initialize",
  "qenv",
  function(.Object, .xData, code = list(), ...) { # nolint: object_name.
    parent <- parent.env(.GlobalEnv)
    new_xdata <- if (rlang::is_missing(.xData)) {
      new.env(parent = parent)
    } else {
      checkmate::assert_environment(.xData)
      rlang::env_clone(.xData, parent = parent)
    }
    lockEnvironment(new_xdata, bindings = TRUE)

    # .xData needs to be unnamed as the `.environment` constructor allows at
    # most 1 unnamed formal argument of class `environment`.
    # See methods::findMethods("initialize")$.environment
    methods::callNextMethod(
      .Object,
      new_xdata, # Mandatory use of unnamed environment arg
      code = code, ...
    )
  }
)

#' It takes a `qenv` class and returns `TRUE` if the input is valid
#' @name qenv-class
#' @keywords internal
setValidity("qenv", function(object) {
  if (any(duplicated(names(object@code)))) {
    "@code must have unique names."
  } else if (!environmentIsLocked(object@.xData)) {
    "@.xData must be locked."
  } else {
    TRUE
  }
})
