# Get code from `qenv`

Retrieves the code stored in the `qenv`.

## Usage

``` r
get_code(object, deparse = TRUE, names = NULL, ...)
```

## Arguments

- object:

  (`qenv`)

- deparse:

  (`logical(1)`) flag specifying whether to return code as `character`
  or `expression`.

- names:

  (`character`) **\[experimental\]** vector of object names to return
  the code for. For more details see the "Extracting dataset-specific
  code" section.

- ...:

  internal usage, please ignore.

## Value

The code used in the `qenv` in the form specified by `deparse`.

## Extracting dataset-specific code

`get_code(object, names)` limits the returned code to contain only those
lines needed to *create* the requested objects. The code stored in the
`qenv` is analyzed statically to determine which lines the objects of
interest depend upon. The analysis works well when objects are created
with standard infix assignment operators (see
[`?assignOps`](https://rdrr.io/r/base/assignOps.html)) but it can fail
in some situations.

Consider the following examples:

*Case 1: Usual assignments.*

    q1 <-
      within(qenv(), {
        foo <- function(x) {
          x + 1
        }
        x <- 0
        y <- foo(x)
      })
    get_code(q1, names = "y")

`x` has no dependencies, so `get_code(data, names = "x")` will return
only the second call.  
`y` depends on `x` and `foo`, so `get_code(data, names = "y")` will
contain all three calls.

*Case 2: Some objects are created by a function's side effects.*

    q2 <-
      within(qenv(){
        foo <- function() {
          x <<- x + 1
        }
        x <- 0
        foo()
        y <- x
      })
    get_code(q2, names = "y")

Here, `y` depends on `x` but `x` is modified by `foo` as a side effect
(not by reassignment) and so `get_code(data, names = "y")` will not
return the `foo()` call.  
To overcome this limitation, code dependencies can be specified
manually. Lines where side effects occur can be flagged by adding
"`# @linksto <object name>`" at the end.  
Note that `within` evaluates code passed to `expr` as is and comments
are ignored. In order to include comments in code one must use the
`eval_code` function instead.

    q3 <-
      eval_code(qenv(), "
        foo <- function() {
          x <<- x + 1
        }
        x <- 0
        foo() # @linksto x
        y <- x
      ")
    get_code(q3, names = "y")

Now the `foo()` call will be properly included in the code required to
recreate `y`.

Note that two functions that create objects as side effects, `assign`
and `data`, are handled automatically.

Here are known cases where manual tagging is necessary:

- non-standard assignment operators, *e.g.* `%<>%`

- objects used as conditions in `if` statements: `if (<condition>)`

- objects used to iterate over in `for` loops: `for(i in <sequence>)`

- creating and evaluating language objects, *e.g.* `eval(<call>)`

## Examples

``` r
# retrieve code
q <- within(qenv(), {
  a <- 1
  b <- 2
})
get_code(q)
#> [1] "a <- 1\nb <- 2"
get_code(q, deparse = FALSE)
#> expression({
#> a <- 1
#> b <- 2
#> })
get_code(q, names = "a")
#> [1] "a <- 1"

q <- qenv()
q <- eval_code(q, code = c("a <- 1", "b <- 2"))
get_code(q, names = "a")
#> [1] "a <- 1"
```
