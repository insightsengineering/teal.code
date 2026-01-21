# Concatenate two `qenv` objects

Combine two `qenv` objects by simple concatenate their environments and
the code.

## Usage

``` r
concat(x, y)
```

## Arguments

- x:

  (`qenv`)

- y:

  (`qenv`)

## Value

`qenv` object.

## Details

We recommend to use the `join` method to have a stricter control in case
`x` and `y` contain duplicated bindings and code. RHS argument content
has priority over the LHS one.

## Examples

``` r
q <- qenv()
q1 <- eval_code(q, expression(iris1 <- iris, mtcars1 <- mtcars))
q2 <- q1
q1 <- eval_code(q1, "iris2 <- iris")
q2 <- eval_code(q2, "mtcars2 <- mtcars")
qq <- concat(q1, q2)
get_code(qq)
#> [1] "iris1 <- iris\nmtcars1 <- mtcars\niris2 <- iris\niris1 <- iris\nmtcars1 <- mtcars\nmtcars2 <- mtcars"
```
