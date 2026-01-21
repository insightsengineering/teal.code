# Access environment included in `qenv`

The access of environment included in the `qenv` that contains all data
objects.

## Usage

``` r
get_env(object)
```

## Arguments

- object:

  (`qenv`).

## Value

An `environment` stored in `qenv` with all data objects.

## Examples

``` r
q <- qenv()
q1 <- within(q, {
  a <- 5
  b <- data.frame(x = 1:10)
})
get_env(q1)
#> <environment: 0x557144517c50>
```
