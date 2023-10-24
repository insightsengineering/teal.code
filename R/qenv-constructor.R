#' Initialize `qenv` object
#'
#' Create an empty `qenv` object.
#'
#' Instantiates a `qenv` with an empty environment.
#' Any changes must be made by evaluating code in it with `eval_code` or `within`,
#' thereby ensuring reproducibility.
#'
#' @name new_qenv
#'
#' @examples
#' new_qenv()
#'
#' @return `qenv` object.
#'
#' @export
new_qenv <- function() {
  q_env <- new.env(parent = parent.env(.GlobalEnv))
  lockEnvironment(q_env, bindings = TRUE)
  methods::new("qenv", env = q_env)
}
