# Separate calls

Converts language object or lists of language objects to list of simple
calls.

## Usage

``` r
lang2calls(x)
```

## Arguments

- x:

  `language` object or a list of thereof

## Value

Given a `call`, an `expression`, a list of `call`s or a list of
`expression`s, returns a list of `calls`. Symbols and atomic vectors
(which may get mixed up in a list) are returned wrapped in list.

## Examples

``` r
# use non-exported function from teal.code
lang2calls <- getFromNamespace("lang2calls", "teal.code")
expr <- expression(
  i <- iris,
  m <- mtcars
)
lang2calls(expr)
#> [[1]]
#> i <- iris
#> 
#> [[2]]
#> m <- mtcars
#> 
```
