# Get messages from `qenv` object

Retrieve all messages raised during code evaluation in a `qenv`.

## Usage

``` r
get_messages(object)
```

## Arguments

- object:

  (`qenv`)

## Value

`character` containing warning information or `NULL` if no messages.

## Examples

``` r
data_q <- qenv()
data_q <- eval_code(data_q, "iris_data <- iris")
warning_qenv <- eval_code(
  data_q,
  bquote(p <- hist(iris_data[, .("Sepal.Length")], ff = ""))
)
cat(get_messages(warning_qenv))
```
