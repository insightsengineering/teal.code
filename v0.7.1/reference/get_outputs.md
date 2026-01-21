# Get outputs

**\[experimental\]**

`eval_code` evaluates code silently so plots and prints don't show up in
the console or graphic devices. If one wants to use an output outside of
the `qenv` (e.g. use a graph in `renderPlot`) then use `get_outputs`.

## Usage

``` r
get_outputs(object)
```

## Arguments

- object:

  (`qenv`)

## Value

list of outputs generated in a \`qenv“

## Examples

``` r
q <- eval_code(
  qenv(),
  quote({
    a <- 1
    print("I'm an output")
    plot(1)
  })
)
get_outputs(q)

#> [[1]]
#> [1] "[1] \"I'm an output\"\n"
#> 
#> [[2]]
#> 
```
