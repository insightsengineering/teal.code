#' Initialize `Quosure` object
#'
#' Initialize `Quosure` object with `code` and `env`. In order to have `Quosure` reproducible
#' one needs to initialize with `env` which can be reproduced by the `code`. Alternatively, one
#' can create an empty `Quosure` and evaluate the expressions in this object using `eval_code`.
#' @name new_quosure
#'
#' @param code (`character(1)` or `language`) code to evaluate. Accepts and stores comments also.
#' @param env (`environment`) Environment being a result of the `code` evaluation.
#'
#' @examples
#' new_quosure(env = list2env(list(a = 1)), code = quote(a <- 1))
#' new_quosure(env = list2env(list(a = 1)), code = parse(text = "a <- 1"))
#'
#' @export
setGeneric("new_quosure", function(env = new.env(parent = parent.env(.GlobalEnv)), code = expression()) {
  standardGeneric("new_quosure")
})

#' @rdname new_quosure
#' @export
setMethod(
  "new_quosure",
  signature = c(env = "environment", code = "character"),
  function(env, code) {
    new_quosure(env, code = parse(text = code, keep.source = FALSE))
  }
)

#' @rdname new_quosure
#' @export
setMethod(
  "new_quosure",
  signature = c(env = "environment", code = "expression"),
  function(env, code) {
    new_env <- rlang::env_clone(env, parent = parent.env(.GlobalEnv))
    lockEnvironment(new_env, bindings = TRUE)
    id <- sample.int(.Machine$integer.max, size = length(code))
    methods::new("Quosure", env = new_env, code = code, id = id)
  }
)

#' @rdname new_quosure
#' @export
setMethod(
  "new_quosure",
  signature = c(env = "environment", code = "language"),
  function(env, code) {
    code_expr <- as.expression(code)
    new_quosure(env = env, code = code_expr)
  }
)

#' @rdname new_quosure
#' @export
setMethod(
  "new_quosure",
  signature = c(env = "list"),
  function(env, code) {
    if (!missing(code)) {
      warning(
        "`code` argument is ignored when creating Quosure from the list.",
        "\nPlease pass the code througn attr(<list>, 'code')"
      )
    }

    code <- attr(env, "code") # named
    if (length(code) == 0) {
      stop("To create the Quosure from the list it must contain 'code' attribute.")
    }

    if (checkmate::test_list(env, "reactive")) {
      env <- lapply(env, function(x) {
        if (inherits(x, "reactive")) {
          x()
        } else {
          x
        }
      })
    }
    new_quosure(env = list2env(env), code = code)
  }
)

#' @rdname new_quosure
#' @export
setMethod(
  "new_quosure",
  signature = c(code = "missing", env = "missing"),
  function(env, code) {
    new_quosure(env = env, code = code)
  }
)
