
# recursively convert language object ot list of simple calls
lang2calls <- function(x) {
  if (is.call(x)) {
    if (identical(as.list(x)[[1L]], as.symbol("{"))) {
      as.list(x)[-1L]
    } else {
      list(x)
    }
  }
  else {
    unlist(lapply(x, lang2calls))
  }
}

# format expression into one string, generally one call per line
format_expression <- function(code) {
  code <- lang2calls(code)
  paste(code, collapse = "\n")
}

# examples
expr1 <- expression({
  i <- iris
  m <- mtcars
})
expr2 <- expression(
  i <- iris,
  m <- mtcars
)
expr3 <- list(
  expression(i <- iris),
  expression(m <- mtcars)
)
cll1 <- substitute({
  i <- iris
  m <- mtcars
})
cll2 <- list(
  quote(i <- iris),
  quote(m <- mtcars)
)

# function definition
fundef <- quote(
  format_expression <- function(x) {
    x + x
    return(x)
  }
)

format_expression(expr1)
format_expression(expr2)
format_expression(expr3)
format_expression(cll1)
format_expression(cll2)
format_expression(fundef)
