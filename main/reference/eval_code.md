# Evaluate code in `qenv`

Evaluate code in `qenv`

## Usage

``` r
eval_code(object, code, ...)
```

## Arguments

- object:

  (`qenv`)

- code:

  (`character`, `language` or `expression`) code to evaluate. It is
  possible to preserve original formatting of the `code` by providing a
  `character` or an `expression` being a result of
  `parse(keep.source = TRUE)`.

- ...:

  ([`dots`](https://rdrr.io/r/base/dots.html)) additional arguments
  passed to future methods.

## Value

`qenv` environment with `code/expr` evaluated or `qenv.error` if
evaluation fails.

## Details

`eval_code()` evaluates given code in the `qenv` environment and appends
it to the `code` slot. Thus, if the `qenv` had been instantiated empty,
contents of the environment are always a result of the stored code.

## See also

[within.qenv](https://insightsengineering.github.io/teal.code/reference/within.qenv.md)

## Examples

``` r
# evaluate code in qenv
q <- qenv()
q <- eval_code(q, "a <- 1")
q <- eval_code(q, "b <- 2L # with comment")
q <- eval_code(q, quote(library(checkmate)))
q <- eval_code(q, expression(assert_number(a)))
```
