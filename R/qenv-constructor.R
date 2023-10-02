#' Initialize `qenv` object
#'
#' Initialize `qenv` object with `code` and `env`. In order to have `qenv` reproducible
#' one needs to initialize with `env` which can be reproduced by the `code`. Alternatively, one
#' can create an empty `qenv` and evaluate the expressions in this object using `eval_code`.
#' @name new_qenv
#'
#' @param code (`character(1)` or `language`) code to evaluate. Accepts and stores comments also.
#' @param env (`environment`) Environment being a result of the `code` evaluation.
#'
#' @examples
#' new_qenv(env = list2env(list(a = 1)), code = quote(a <- 1))
#' new_qenv(env = list2env(list(a = 1)), code = parse(text = "a <- 1"))
#' new_qenv(env = list2env(list(a = 1)), code = "a <- 1")
#'
#' @return `qenv` object.
#'
#' @export
setGeneric("new_qenv", function(env = new.env(parent = parent.env(.GlobalEnv)), code = expression()) standardGeneric("new_qenv")) # nolint

#' @rdname new_qenv
#' @export
setMethod(
  "new_qenv",
  signature = c(env = "environment", code = "expression"),
  function(env, code) {
    new_qenv(env, as.character(code))
  }
)

#' @rdname new_qenv
#' @export
setMethod(
  "new_qenv",
  signature = c(env = "environment", code = "character"),
  function(env, code) {
    new_env <- rlang::env_clone(env, parent = parent.env(.GlobalEnv))
    lockEnvironment(new_env, bindings = TRUE)
    code <- paste(code, collapse = "\n")
    id <- sample.int(.Machine$integer.max, size = 1)
    methods::new("qenv", env = new_env, code = code, warnings = "", messages = "", id = id)
  }

)

#' @rdname new_qenv
#' @export
setMethod(
  "new_qenv",
  signature = c(env = "environment", code = "language"),
  function(env, code) {
    new_qenv(env = env, code = as.character(as.expression(code)))
  }
)

#' @rdname new_qenv
#' @export
setMethod(
  "new_qenv",
  signature = c(code = "missing", env = "missing"),
  function(env, code) {
    new_qenv(env = env, code = code)
  }
)
