# Display `qenv` object

Prints the `qenv` object.

## Usage

``` r
# S4 method for class 'qenv'
show(object)
```

## Arguments

- object:

  (`qenv`)

## Value

`object`, invisibly.

## Examples

``` r
q <- qenv()
q1 <- eval_code(q, expression(a <- 5, b <- data.frame(x = 1:10)))
q1
#> <environment: 0x5625815f68d8> 🔒 
#> Parent: <environment: package:checkmate> 
#> Bindings:
#> - a: [numeric]
#> - b: [data.frame]
```
