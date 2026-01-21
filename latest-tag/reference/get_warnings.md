# Get warnings from `qenv` object

Retrieve all warnings raised during code evaluation in a `qenv`.

## Usage

``` r
get_warnings(object)
```

## Arguments

- object:

  (`qenv`)

## Value

`character` containing warning information or `NULL` if no warnings.

## Examples

``` r
data_q <- qenv()
data_q <- eval_code(data_q, "iris_data <- iris")
warning_qenv <- eval_code(
  data_q,
  bquote(p <- hist(iris_data[, .("Sepal.Length")], ff = ""))
)
cat(get_warnings(warning_qenv))
#> ~~~ Warnings ~~~
#> 
#> > "ff" is not a graphical parameter
#> when running code:
#> p <- hist(iris_data[, "Sepal.Length"], ff = "")
#> 
#> > "ff" is not a graphical parameter
#> when running code:
#> p <- hist(iris_data[, "Sepal.Length"], ff = "")
#> 
#> > "ff" is not a graphical parameter
#> when running code:
#> p <- hist(iris_data[, "Sepal.Length"], ff = "")
#> 
#> > "ff" is not a graphical parameter
#> when running code:
#> p <- hist(iris_data[, "Sepal.Length"], ff = "")
#> 
#> ~~~ Trace ~~~
#> 
#> iris_data <- iris
#> p <- hist(iris_data[, "Sepal.Length"], ff = "")
```
