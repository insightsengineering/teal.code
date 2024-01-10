#' Code tracking with `qenv` object
#'
#' @description
#' `r badge("stable")`
#'
#' Create a `qenv` object and evaluate code in it to track code history.
#'
#' @details
#'
#' `qenv()` instantiates a `qenv` with an empty environment.
#' Any changes must be made by evaluating code in it with `eval_code` or `within`, thereby ensuring reproducibility.
#'
#' `new_qenv()` (`r badge("deprecated")` and not recommended)
#' can instantiate a `qenv` object with data in the environment and code registered.
#'
#' @name qenv
#'
#' @return `qenv` and `new_qenv` return a `qenv` object.
#'
#' @examples
#' # create empty qenv
#' qenv()
#'
#' @export
qenv <- function() {
  q_env <- new.env(parent = parent.env(.GlobalEnv))
  lockEnvironment(q_env, bindings = TRUE)
  methods::new("qenv", env = q_env)
}


#' @param code `r badge("deprecated")`
#'  (`character(1)` or `language`) code to evaluate. Accepts and stores comments also.
#' @param env `r badge("deprecated")` (`environment`)
#'  Environment being a result of the `code` evaluation.
#'
#' @examples
#' # create qenv with data and code (deprecated)
#' new_qenv(env = list2env(list(a = 1)), code = quote(a <- 1))
#' new_qenv(env = list2env(list(a = 1)), code = parse(text = "a <- 1", keep.source = TRUE))
#' new_qenv(env = list2env(list(a = 1)), code = "a <- 1")
#'
#' @rdname qenv
#' @aliases new_qenv,environment,expression-method
#' @aliases new_qenv,environment,character-method
#' @aliases new_qenv,environment,language-method
#' @aliases new_qenv,environment,missing-method
#' @aliases new_qenv,missing,missing-method
#'
#' @seealso [`base::within()`], [`get_var()`], [`get_env()`], [`get_warnings()`], [`join()`], [`concat()`]
#'
#' @export
setGeneric("new_qenv", function(env = new.env(parent = parent.env(.GlobalEnv)), code = character()) {
  lifecycle::deprecate_warn(when = " 0.5.0", what = "new_qenv()", with = "qenv()", always = TRUE)
  standardGeneric("new_qenv")
})

setMethod(
  "new_qenv",
  signature = c(env = "environment", code = "expression"),
  function(env, code) {
    new_qenv(env, paste(lang2calls(code), collapse = "\n"))
  }
)

setMethod(
  "new_qenv",
  signature = c(env = "environment", code = "character"),
  function(env, code) {
    new_env <- rlang::env_clone(env, parent = parent.env(.GlobalEnv))
    lockEnvironment(new_env, bindings = TRUE)
    if (length(code) > 0) code <- paste(code, collapse = "\n")
    id <- sample.int(.Machine$integer.max, size = length(code))
    methods::new(
      "qenv",
      env = new_env, code = code, warnings = rep("", length(code)), messages = rep("", length(code)), id = id
    )
  }
)

setMethod(
  "new_qenv",
  signature = c(env = "environment", code = "language"),
  function(env, code) {
    new_qenv(env = env, code = paste(lang2calls(code), collapse = "\n"))
  }
)

setMethod(
  "new_qenv",
  signature = c(code = "missing", env = "missing"),
  function(env, code) {
    new_qenv(env = env, code = code)
  }
)
