#' @export
within.qenv <- function(data, expr, ...) {
  expr <- substitute(expr)
  extras <- list(...)

  # Get parent call to use in error messages.
  parent_call <- deparse1(match.call(definition = sys.function(1L), call = sys.call(1L)))

  if (is.symbol(expr)) {
    warning(
      "In ", parent_call, " : Symbol passed to \"expr\" will be evaluated within qenv as is. ", "\n",
      "This function is intended for literal expressions, for variables containing code use `eval_code` instead.",
      call. = FALSE
    )
  }

  # Add braces for consistency.
  if (!identical(as.list(expr)[[1L]], as.symbol("{"))) {
    expr <- call("{", expr)
  }
  # Drop strings from compound expressions.
  calls <- Filter(Negate(is.character), as.list(expr)[-1])

  # Inject extra values into expressions.
  calls <- lapply(calls, function(x) do.call(substitute, list(x, env = extras)))

  eval_code(object = data, code = as.expression(calls))
}
