# Evaluate code in `qenv`

Evaluate code in `qenv`

## Usage

``` r
# S3 method for class 'qenv'
within(data, expr, ...)
```

## Arguments

- data:

  (`qenv`)

- expr:

  (`expression`) to evaluate. Must be inline code, see
  `Using language objects...`

- ...:

  named argument value will substitute a symbol in the `expr` matched by
  the name. For practical usage see Examples section below.

## Details

[`within()`](https://rdrr.io/r/base/with.html) is a convenience method
that wraps `eval_code` to provide a simplified way of passing
expression. `within` accepts only inline expressions (both simple and
compound) and allows to substitute `expr` with `...` named argument
values. Functions that trigger side effects like `options` or `set.seed`
can be linked to specific objects for further code retrieval (with
`get_code`), but only through `eval_code` where code input as
`character`. `within` works on `expressions` that do not preserve
comments, hence you can not use `# @linksto` tag explained in
`get_code`.

## Using language objects with `within`

Passing language objects to `expr` is generally not intended but can be
achieved with `do.call`. Only single `expression`s will work and
substitution is not available. See examples.

## Examples

``` r
# evaluate code using within
q <- qenv()
q <- within(q, {
  i <- iris
})
q <- within(q, {
  m <- mtcars
  f <- faithful
})
q
#> <environment: 0x558f11f16a08> 🔒 
#> Parent: <environment: package:checkmate> 
#> Bindings:
#> - f: [data.frame]
#> - i: [data.frame]
#> - m: [data.frame]
get_code(q)
#> [1] "i <- iris\nm <- mtcars\nf <- faithful"

# inject values into code
q <- qenv()
q <- within(q, i <- iris)
within(q, print(dim(subset(i, Species == "virginica"))))
#> <environment: 0x558f13678438> 🔒 
#> Parent: <environment: package:checkmate> 
#> Bindings:
#> - i: [data.frame]
within(q, print(dim(subset(i, Species == species)))) # fails
#> <qenv.error: object 'species' not found 
#>  when evaluating qenv code:
#> print(dim(subset(i, Species == species)))>
within(q, print(dim(subset(i, Species == species))), species = "versicolor")
#> <environment: 0x558f15e02f68> 🔒 
#> Parent: <environment: package:checkmate> 
#> Bindings:
#> - i: [data.frame]
species_external <- "versicolor"
within(q, print(dim(subset(i, Species == species))), species = species_external)
#> <environment: 0x558f1272c468> 🔒 
#> Parent: <environment: package:checkmate> 
#> Bindings:
#> - i: [data.frame]

# pass language objects
expr <- expression(i <- iris, m <- mtcars)
within(q, expr) # fails
#> <qenv.error: object 'expr' not found 
#>  when evaluating qenv code:
#> expr>
do.call(within, list(q, expr))
#> <environment: 0x558f14e07a50> 🔒 
#> Parent: <environment: package:checkmate> 
#> Bindings:
#> - i: [data.frame]
#> - m: [data.frame]

exprlist <- list(expression(i <- iris), expression(m <- mtcars))
within(q, exprlist) # fails
#> <qenv.error: object 'exprlist' not found 
#>  when evaluating qenv code:
#> exprlist>
do.call(within, list(q, do.call(c, exprlist)))
#> <environment: 0x558f1683b560> 🔒 
#> Parent: <environment: package:checkmate> 
#> Bindings:
#> - i: [data.frame]
#> - m: [data.frame]
```
