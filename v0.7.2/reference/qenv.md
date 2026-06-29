# Instantiates a `qenv` environment

**\[stable\]**

Instantiates a `qenv` environment.

## Usage

``` r
qenv()
```

## Value

`qenv` environment.

## Details

`qenv` class has following characteristics:

- It inherits from the environment and methods such as
  [`$`](https://rdrr.io/r/base/Extract.html),
  [`get()`](https://rdrr.io/r/base/get.html),
  [`ls()`](https://rdrr.io/r/base/ls.html),
  [`as.list()`](https://rdrr.io/r/base/list.html),
  [`parent.env()`](https://rdrr.io/r/base/environment.html) work out of
  the box.

- `qenv` is a locked environment, and data modification is only possible
  through the
  [`eval_code()`](https://insightsengineering.github.io/teal.code/reference/eval_code.md)
  and
  [`within.qenv()`](https://insightsengineering.github.io/teal.code/reference/within.qenv.md)
  functions.

- It stores metadata about the code used to create the data (see
  [`get_code()`](https://insightsengineering.github.io/teal.code/reference/get_code.md)).

- It supports slicing (see
  [`subset-qenv`](https://insightsengineering.github.io/teal.code/reference/subset-qenv.md))

- It is immutable which means that each code evaluation does not modify
  the original `qenv` environment directly. See the following code:

      q1 <- qenv()
      q2 <- eval_code(q1, "a <- 1")
      identical(q1, q2) # FALSE

## See also

[`eval_code()`](https://insightsengineering.github.io/teal.code/reference/eval_code.md),
[`get_var()`](https://insightsengineering.github.io/teal.code/reference/get_var.md),
[`subset-qenv`](https://insightsengineering.github.io/teal.code/reference/subset-qenv.md),
[`get_env()`](https://insightsengineering.github.io/teal.code/reference/get_env.md),[`get_warnings()`](https://insightsengineering.github.io/teal.code/reference/get_warnings.md),
[`join()`](https://insightsengineering.github.io/teal.code/reference/join.md),
[`concat()`](https://insightsengineering.github.io/teal.code/reference/concat.md)

## Examples

``` r
q <- qenv()
q2 <- within(q, a <- 1)
ls(q2)
#> [1] "a"
q2$a
#> [1] 1
```
