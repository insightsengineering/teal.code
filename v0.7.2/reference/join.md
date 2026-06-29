# Join `qenv` objects

**\[deprecated\]** Instead of `join()` use
[`c()`](https://rdrr.io/r/base/c.html).

## Usage

``` r
# S3 method for class 'qenv'
c(...)

# S3 method for class 'qenv.error'
c(...)

join(...)
```

## Arguments

- ...:

  function is deprecated.

## Examples

``` r
q <- qenv()
q1 <- within(q, {
  iris1 <- iris
  mtcars1 <- mtcars
})
q1 <- within(q1, iris2 <- iris)
q2 <- within(q1, mtcars2 <- mtcars)
qq <- c(q1, q2)
cat(get_code(qq))
#> iris1 <- iris
#> mtcars1 <- mtcars
#> iris2 <- iris
#> mtcars2 <- mtcars
```
