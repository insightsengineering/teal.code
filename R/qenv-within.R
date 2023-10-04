#' Evaluate expression in `qenv` object.
#'
#' Convenience function for evaluating inline code inside the environment of a `qenv`.
#'
#' This is a wrapper for `eval_code` that provides a simplified way of passing code for evaluation.
#' It accepts only inline expressions (both simple and compound) and allows for injecting values into `expr`
#' through the `...` argument: as `name:value` pairs are passed to `...`,
#' `name` in `expr` will be replaced with `value`.
#'
#'
#' @param data `qenv` object
#' @param expr `expression` to evaluate
#' @param ... `name:value` pairs to inject values into `expr`
#'
#' @return
#' Returns a `qenv` object with `expr` evaluated. If evaluation raises an error, a `qenv.error` is returned.
#'
#' @seealso [`eval_code`], [`base::within`]
#'
#' @export
#'
#' @rdname within
#'
#' @examples
#'
#' q <- new_qenv()
#'
#' # execute code
#' q <- within(q, {
#'   i <- iris
#' })
#' q <- within(q, {
#'   m <- mtcars
#'   f <- faithful
#' })
#' q
#' get_code(q)
#'
#' # inject values into code
#' q <- new_qenv()
#' q <- within(q, i <- iris)
#' within(q, print(dim(subset(i, Species == "virginica"))))
#' within(q, print(dim(subset(i, Species == species)))) # fails
#' within(q, print(dim(subset(i, Species == species))), species = "versicolor")
#' species_external <- "versicolor"
#' within(q, print(dim(subset(i, Species == species))), species = species_external)
#'
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