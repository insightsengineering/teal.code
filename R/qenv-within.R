#' Evaluate expression in `qenv` object.
#'
#' Convenience function for evaluating inline code inside the environment of a `qenv`.
#'
#' This is a wrapper for `eval_code` that provides a simplified way of passing code for evaluation.
#' It accepts only inline expressions (both simple and compound) and allows for injecting values into `expr`
#' through the `...` argument: as `name:value` pairs are passed to `...`,
#' `name` in `expr` will be replaced with `value`.
#'
#' @section Using language objects:
#' Passing language objects to `expr` is generally not intended but can be achieved with `do.call`.
#' Only single `expression`s will work and substitution is not available. See examples.
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
#' q <- qenv()
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
#' q <- qenv()
#' q <- within(q, i <- iris)
#' within(q, print(dim(subset(i, Species == "virginica"))))
#' within(q, print(dim(subset(i, Species == species)))) # fails
#' within(q, print(dim(subset(i, Species == species))), species = "versicolor")
#' species_external <- "versicolor"
#' within(q, print(dim(subset(i, Species == species))), species = species_external)
#'
#' # pass language objects
#' expr <- expression(i <- iris, m <- mtcars)
#' within(q, expr) # fails
#' do.call(within, list(q, expr))
#'
#' exprlist <- list(expression(i <- iris), expression(m <- mtcars))
#' within(q, exprlist) # fails
#' do.call(within, list(q, do.call(c, exprlist)))
#'
within.qenv <- function(data, expr, ...) {
  expr <- substitute(expr)
  extras <- list(...)

  # Add braces for consistency.
  if (!identical(as.list(expr)[[1L]], as.symbol("{"))) {
    expr <- call("{", expr)
  }

  calls <- as.list(expr)[-1]

  # Inject extra values into expressions.
  calls <- lapply(calls, function(x) do.call(substitute, list(x, env = extras)))

  eval_code(object = data, code = as.expression(calls))
}
