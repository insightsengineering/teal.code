#' @details
#' `within()` is a convenience method that wraps `eval_code` to provide a simplified way of passing expression.
#' `within` accepts only inline expressions (both simple and compound) and allows to substitute `expr`
#' with `...` named argument values.
#'
#' @section Using language objects with `within`:
#' Passing language objects to `expr` is generally not intended but can be achieved with `do.call`.
#' Only single `expression`s will work and substitution is not available. See examples.
#'
#' @param data (`qenv`)
#' @param expr (`expression`) to evaluate. Must be inline code, see `Using language objects...`
#' @param ... named argument value will substitute a symbol in the `expr` matched by the name.
#' For practical usage see Examples section below.
#'
#' @examples
#' # evaluate code using within
#' q <- qenv()
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
#' @rdname eval_code
#'
#' @export
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


#' @keywords internal
#'
#' @export
within.qenv.error <- function(data, expr, ...) {
  data
}
