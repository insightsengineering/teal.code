# Get code dependency of an object

Extract subset of code required to reproduce specific object(s),
including code producing side-effects.

## Usage

``` r
get_code_dependency(code, names, check_code_names = TRUE)
```

## Arguments

- code:

  `character` with the code.

- names:

  `character` vector of object names.

- check_code_names:

  `logical(1)` flag specifying if a warning for non-existing names
  should be displayed.

## Value

Character vector, a subset of `code`. Note that subsetting is actually
done on the calls `code`, not necessarily on the elements of the vector.

## Details

Given a character vector with code, this function will extract the part
of the code responsible for creating the variables specified by `names`.
This includes the final call that creates the variable(s) in question as
well as all *parent calls*, *i.e.* calls that create variables used in
the final call and their parents, etc. Also included are calls that
create side-effects like establishing connections.

It is assumed that object dependency is established by using three
assignment operators: `<-`, `=`, and `->` . Other assignment methods
(`assign`, `<<-`) or non-standard-evaluation methods are not supported.

Side-effects are not detected automatically and must be marked in the
code. Add `# @linksto object` at the end of a line where a side-effect
occurs to specify that this line is required to reproduce a variable
called `object`.
