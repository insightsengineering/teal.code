# Subsets `qenv`

Subsets
[`qenv`](https://insightsengineering.github.io/teal.code/reference/qenv.md)
environment and limits the code to the necessary needed to build limited
objects.

## Usage

``` r
# S3 method for class 'qenv'
x[names, ...]
```

## Arguments

- x:

  (`qenv`)

- names:

  (`character`) names of objects included in
  [`qenv`](https://insightsengineering.github.io/teal.code/reference/qenv.md)
  to subset. Names not present in
  [`qenv`](https://insightsengineering.github.io/teal.code/reference/qenv.md)
  are skipped.

- ...:

  internal usage, please ignore.

## Examples

``` r
q <- qenv()
q <- eval_code(q, "a <- 1;b<-2")
q["a"]
#> <environment: 0x56188bff1178> 🔒 
#> Parent: <environment: package:checkmate> 
#> Bindings:
#> - a: [numeric]
q[c("a", "b")]
#> <environment: 0x56188ce2d1c0> 🔒 
#> Parent: <environment: package:checkmate> 
#> Bindings:
#> - a: [numeric]
#> - b: [numeric]
```
