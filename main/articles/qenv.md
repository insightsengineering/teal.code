# qenv

## Introduction to `qenv`

A `qenv` inherits from the `environment` class, behaves like an
environment, and has the following characteristics:

- It inherits from the environment and methods such as `$`, `get`, `ls`,
  [`as.list()`](https://rdrr.io/r/base/list.html) work out of the box.
- `qenv` is a locked environment, and data modification is only possible
  through the `eval_code` and `within` functions.
- It stores printed and plotted outputs (see `get_outputs`).
- It stores metadata about the code used to create the data (see
  `get_code`).
- It supports slicing by `[`.
- It is immutable which means that each code evaluation does not modify
  the original `qenv` environment directly.

### Initialization

The
[`qenv()`](https://insightsengineering.github.io/teal.code/reference/qenv.md)
function serves as the gateway to create an initial `qenv` object:

[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.code`](https://insightsengineering.github.io/teal.code/)`)`` `` ``# create a new qenv object`` ``empty_qenv`` ``<-`` `[`qenv`](https://insightsengineering.github.io/teal.code/reference/qenv.md)`(``)`` `[`print`](https://rdrr.io/r/base/print.html)`(``empty_qenv``)`

    ## <environment: 0x55f5ef41f348> 🔒 
    ## Parent: <environment: package:teal.code>

### `qenv` basic usage

To modify the data use `eval_code` to execute R code within the
environment, yielding a new `qenv` object as the output.

`# evaluate code in qenv`` ``my_qenv`` ``<-`` `[`eval_code`](https://insightsengineering.github.io/teal.code/reference/eval_code.md)`(``empty_qenv``, ``"x <- 2"``)`` `[`print`](https://rdrr.io/r/base/print.html)`(``my_qenv``)`

    ## <environment: 0x55f5f06a5328> 🔒 
    ## Parent: <environment: package:teal.code> 
    ## Bindings:
    ## - x: [numeric]

`q1`` ``<-`` `[`eval_code`](https://insightsengineering.github.io/teal.code/reference/eval_code.md)`(``my_qenv``, ``"y <- x * 2"``)`` ``q1`` ``<-`` `[`eval_code`](https://insightsengineering.github.io/teal.code/reference/eval_code.md)`(``q1``, ``"z <- y * 2"``)`` `` ``# my_qenv still contains only x`` `[`print`](https://rdrr.io/r/base/print.html)`(``my_qenv``)`

    ## <environment: 0x55f5f06a5328> 🔒 
    ## Parent: <environment: package:teal.code> 
    ## Bindings:
    ## - x: [numeric]

[`names`](https://rdrr.io/r/base/names.html)`(``my_qenv``)`

    ## [1] "x"

`# q1 contains x, y and z`` `[`print`](https://rdrr.io/r/base/print.html)`(``q1``)`

    ## <environment: 0x55f5f14ef468> 🔒 
    ## Parent: <environment: package:teal.code> 
    ## Bindings:
    ## - x: [numeric]
    ## - y: [numeric]
    ## - z: [numeric]

[`names`](https://rdrr.io/r/base/names.html)`(``q1``)`

    ## [1] "x" "y" "z"

The same result can be achieved with the `within` method.

`q2`` ``<-`` `[`within`](https://insightsengineering.github.io/teal.code/reference/within.qenv.md)`(``my_qenv``, ``y`` ``<-`` ``x`` ``*`` ``2``)`` ``q2`` ``<-`` `[`within`](https://insightsengineering.github.io/teal.code/reference/within.qenv.md)`(``q2``, ``z`` ``<-`` ``y`` ``*`` ``2``)`` ``q2`` ``<-`` `[`within`](https://insightsengineering.github.io/teal.code/reference/within.qenv.md)`(``q2``, `[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``z``)``)`` `[`print`](https://rdrr.io/r/base/print.html)`(``q2``)`

    ## <environment: 0x55f5f0831cb8> 🔒 
    ## Parent: <environment: package:teal.code> 
    ## Bindings:
    ## - x: [numeric]
    ## - y: [numeric]
    ## - z: [numeric]

To extract specific object from a `qenv`’s environment, use `[[`. To
extract an output of a `print` or `plot` functions, use
[`get_outputs()`](https://insightsengineering.github.io/teal.code/reference/get_outputs.md)
to get a [`list()`](https://rdrr.io/r/base/list.html) of outputs
captured by `qenv`. These functions are particularly useful for
displaying them in a `shiny` app. You can retrieve the code used to
generate the `qenv` using the
[`get_code()`](https://insightsengineering.github.io/teal.code/reference/get_code.md)
function.

[`print`](https://rdrr.io/r/base/print.html)`(``q2``[[``"y"``]``]``)`

    ## [1] 4

[`print`](https://rdrr.io/r/base/print.html)`(`[`get_outputs`](https://insightsengineering.github.io/teal.code/reference/get_outputs.md)`(``q2``)``[[``1``]``]``)`

![](qenv_files/figure-html/unnamed-chunk-4-1.png)

[`cat`](https://rdrr.io/r/base/cat.html)`(`[`get_code`](https://insightsengineering.github.io/teal.code/reference/get_code.md)`(``q2``)``)`

    ## x <- 2
    ## y <- x * 2
    ## z <- y * 2
    ## plot(z)

### Substitutions

In some cases, one may want to substitute some elements of the code
before evaluation. Consider a case when a subset of `iris` is defined by
an input value.

`q`` ``<-`` `[`qenv`](https://insightsengineering.github.io/teal.code/reference/qenv.md)`(``)`` ``q`` ``<-`` `[`eval_code`](https://insightsengineering.github.io/teal.code/reference/eval_code.md)`(``q``, `[`quote`](https://rdrr.io/r/base/substitute.html)`(``i`` ``<-`` `[`subset`](https://rdrr.io/r/base/subset.html)`(``iris``, ``Species`` ``==`` ``"setosa"``)``)``)`` ``q`` ``<-`` `[`eval_code`](https://insightsengineering.github.io/teal.code/reference/eval_code.md)`(``q``, `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`` `` ``ii`` ``<-`` `[`subset`](https://rdrr.io/r/base/subset.html)`(``iris``, ``Species`` ``==`` ``species``)``,`` `` env ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``species ``=`` ``"versicolor"``)`` ``)``)`` ``input_value`` ``<-`` ``"virginica"`` ``q`` ``<-`` `[`eval_code`](https://insightsengineering.github.io/teal.code/reference/eval_code.md)`(``q``, `[`substitute`](https://rdrr.io/r/base/substitute.html)`(`` `` ``iii`` ``<-`` `[`subset`](https://rdrr.io/r/base/subset.html)`(``iris``, ``Species`` ``==`` ``species``)``,`` `` env ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``species ``=`` ``input_value``)`` ``)``)`` `` `[`summary`](https://rdrr.io/r/base/summary.html)`(``q``[[``"i"``]``]``$``Species``)`

    ##     setosa versicolor  virginica 
    ##         50          0          0

[`summary`](https://rdrr.io/r/base/summary.html)`(``q``[[``"ii"``]``]``$``Species``)`

    ##     setosa versicolor  virginica 
    ##          0         50          0

[`summary`](https://rdrr.io/r/base/summary.html)`(``q``[[``"iii"``]``]``$``Species``)`

    ##     setosa versicolor  virginica 
    ##          0          0         50

A more convenient way to pass code with substitution is to use the
`within` method.

`qq`` ``<-`` `[`qenv`](https://insightsengineering.github.io/teal.code/reference/qenv.md)`(``)`` ``qq`` ``<-`` `[`within`](https://insightsengineering.github.io/teal.code/reference/within.qenv.md)`(``qq``, ``i`` ``<-`` `[`subset`](https://rdrr.io/r/base/subset.html)`(``iris``, ``Species`` ``==`` ``"setosa"``)``)`` ``qq`` ``<-`` `[`within`](https://insightsengineering.github.io/teal.code/reference/within.qenv.md)`(``qq``, ``ii`` ``<-`` `[`subset`](https://rdrr.io/r/base/subset.html)`(``iris``, ``Species`` ``==`` ``species``)``, species ``=`` ``"versicolor"``)`` ``input_value`` ``<-`` ``"virginica"`` ``qq`` ``<-`` `[`within`](https://insightsengineering.github.io/teal.code/reference/within.qenv.md)`(``qq``, ``iii`` ``<-`` `[`subset`](https://rdrr.io/r/base/subset.html)`(``iris``, ``Species`` ``==`` ``species``)``, species ``=`` ``input_value``)`` `` `[`summary`](https://rdrr.io/r/base/summary.html)`(``qq``[[``"i"``]``]``$``Species``)`

    ##     setosa versicolor  virginica 
    ##         50          0          0

[`summary`](https://rdrr.io/r/base/summary.html)`(``qq``[[``"ii"``]``]``$``Species``)`

    ##     setosa versicolor  virginica 
    ##          0         50          0

[`summary`](https://rdrr.io/r/base/summary.html)`(``qq``[[``"iii"``]``]``$``Species``)`

    ##     setosa versicolor  virginica 
    ##          0          0         50

See
[`?qenv`](https://insightsengineering.github.io/teal.code/reference/qenv.md)
for more details.

### Combining `qenv` objects

Given a pair of `qenv` objects, you may be able to “join” them, creating
a new `qenv` object encompassing the union of both environments, along
with the requisite code for reproduction:

`common_q`` ``<-`` `[`eval_code`](https://insightsengineering.github.io/teal.code/reference/eval_code.md)`(`[`qenv`](https://insightsengineering.github.io/teal.code/reference/qenv.md)`(``)``, `[`quote`](https://rdrr.io/r/base/substitute.html)`(``x`` ``<-`` ``1``)``)`` `` ``x_q`` ``<-`` `[`eval_code`](https://insightsengineering.github.io/teal.code/reference/eval_code.md)`(``common_q``, `[`quote`](https://rdrr.io/r/base/substitute.html)`(``y`` ``<-`` ``5``)``)`` ``y_q`` ``<-`` `[`eval_code`](https://insightsengineering.github.io/teal.code/reference/eval_code.md)`(``common_q``, `[`quote`](https://rdrr.io/r/base/substitute.html)`(``z`` ``<-`` ``5``)``)`` `` ``join_q`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``x_q``, ``y_q``)`` `` `[`print`](https://rdrr.io/r/base/print.html)`(``join_q``)`

    ## <environment: 0x55f5ef2cc8c8> 🔒 
    ## Parent: <environment: package:teal.code> 
    ## Bindings:
    ## - x: [numeric]
    ## - y: [numeric]
    ## - z: [numeric]

[`names`](https://rdrr.io/r/base/names.html)`(``join_q``)`

    ## [1] "x" "y" "z"

The feasibility of joining `qenv` objects hinges on the contents of the
environments and the code’s order. Refer to the function documentation
for further details.

### Warnings and messages in `qenv` objects

In cases where warnings or messages arise while evaluating code within a
`qenv` environment, these are captured and stored within the `qenv`
object. Access these messages and warnings using
[`get_messages()`](https://insightsengineering.github.io/teal.code/reference/get_messages.md)
and
[`get_warnings()`](https://insightsengineering.github.io/teal.code/reference/get_warnings.md)
functions as shown below.

`q_message`` ``<-`` `[`eval_code`](https://insightsengineering.github.io/teal.code/reference/eval_code.md)`(`[`qenv`](https://insightsengineering.github.io/teal.code/reference/qenv.md)`(``)``, `[`quote`](https://rdrr.io/r/base/substitute.html)`(`[`message`](https://rdrr.io/r/base/message.html)`(``"this is a message"``)``)``)`` `[`get_messages`](https://insightsengineering.github.io/teal.code/reference/get_messages.md)`(``q_message``)`

    ## [1] "~~~ Messages ~~~\n\n> this is a message\nwhen running code:\nmessage(\"this is a message\")\n\n~~~ Trace ~~~\n\nmessage(\"this is a message\")"

`q_warning`` ``<-`` `[`eval_code`](https://insightsengineering.github.io/teal.code/reference/eval_code.md)`(`[`qenv`](https://insightsengineering.github.io/teal.code/reference/qenv.md)`(``)``, `[`quote`](https://rdrr.io/r/base/substitute.html)`(`[`warning`](https://rdrr.io/r/base/warning.html)`(``"and this is a warning"``)``)``)`` `[`get_warnings`](https://insightsengineering.github.io/teal.code/reference/get_warnings.md)`(``q_warning``)`

    ## [1] "~~~ Warnings ~~~\n\n> and this is a warning\nwhen running code:\nwarning(\"and this is a warning\")\n\n~~~ Trace ~~~\n\nwarning(\"and this is a warning\")"

If any of above returns `NULL`m then no warnings nor messages were
present.

## Utilizing `qenv` inside `shiny` applications

These functions can be seamlessly integrated into `shiny` applications
to produce reproducible outputs. In the example below, the `rcode`
section showcases the code employed for generating the output.

When employing a `qenv` to evaluate code, should an error occur, an
object of type `qenv.error` is generated. This object can be utilized
wherever a `qenv` object is used, alleviating the need for code
alterations to handle these errors. Select the `error_option` in the
example below to witness `qenv` error handling in action.

[`library`](https://rdrr.io/r/base/library.html)`(`[`shiny`](https://shiny.posit.co/)`)`` ``# create an initial qenv with the data in`` ``data_q`` ``<-`` `[`qenv`](https://insightsengineering.github.io/teal.code/reference/qenv.md)`(``)`` ``data_q`` ``<-`` `[`eval_code`](https://insightsengineering.github.io/teal.code/reference/eval_code.md)`(``data_q``, ``"iris_data <- iris"``)`` `` ``ui`` ``<-`` `[`fluidPage`](https://rdrr.io/pkg/shiny/man/fluidPage.html)`(`` `` `[`radioButtons`](https://rdrr.io/pkg/shiny/man/radioButtons.html)`(`` `` ``"option"``, ``"Choose a column to plot:"``,`` `` `[`c`](https://rdrr.io/r/base/c.html)`(``"Sepal.Length"``, ``"Sepal.Width"``, ``"Petal.Length"``, ``"Petal.Width"``, ``"error_option"``)`` `` ``)``,`` `` `[`verbatimTextOutput`](https://rdrr.io/pkg/shiny/man/textOutput.html)`(``"rcode"``)``,`` `` `[`plotOutput`](https://rdrr.io/pkg/shiny/man/plotOutput.html)`(``"plot"``)`` ``)`` `` ``server`` ``<-`` ``function``(``input``, ``output``, ``session``)`` ``{`` `` ``# create a qenv containing the reproducible output`` `` ``output_q`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`` `` `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``input``$``option``)`` `` `[`within`](https://insightsengineering.github.io/teal.code/reference/within.qenv.md)`(`` `` ``data_q``,`` `` ``p`` ``<-`` `[`hist`](https://rdrr.io/r/graphics/hist.html)`(``iris_data``[``, ``.``(``input``$``option``)``]``)`` `` ``)`` `` ``}``)`` `` `` ``# display plot output`` `` ``output``$``plot`` ``<-`` `[`renderPlot`](https://rdrr.io/pkg/shiny/man/renderPlot.html)`(``output_q``(``)``[[``"p"``]``]``)`` `` ``# display code`` `` ``output``$``rcode`` ``<-`` `[`renderText`](https://rdrr.io/pkg/shiny/man/renderPrint.html)`(`[`get_code`](https://insightsengineering.github.io/teal.code/reference/get_code.md)`(``output_q``(``)``)``)`` ``}`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` `[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``ui``, ``server``)`` ``}`

### Reproducibility

The code inside a `qenv` object can be retrieved using `get_code`
function.

`q_reproducible`` ``<-`` `[`qenv`](https://insightsengineering.github.io/teal.code/reference/qenv.md)`(``)`` ``q_reproducible`` ``<-`` `[`within`](https://insightsengineering.github.io/teal.code/reference/within.qenv.md)`(``q_reproducible``, ``{`` `` ``a`` ``<-`` ``2`` `` ``b`` ``<-`` ``5`` `` ``c`` ``<-`` ``a`` ``+`` ``b`` ``}``)`` `[`cat`](https://rdrr.io/r/base/cat.html)`(`[`get_code`](https://insightsengineering.github.io/teal.code/reference/get_code.md)`(``q_reproducible``)``)`

    ## a <- 2
    ## b <- 5
    ## c <- a + b

[`cat`](https://rdrr.io/r/base/cat.html)`(`[`get_code`](https://insightsengineering.github.io/teal.code/reference/get_code.md)`(``q_reproducible``, names ``=`` ``"a"``)``)`

    ## a <- 2

[`cat`](https://rdrr.io/r/base/cat.html)`(`[`get_code`](https://insightsengineering.github.io/teal.code/reference/get_code.md)`(``q_reproducible``, names ``=`` ``"c"``)``)`

    ## a <- 2
    ## b <- 5
    ## c <- a + b

As demonstrated, you can retrieve the code responsible for creating
specific objects by passing the object’s name to the `names` argument in
`get_code`. In scenarios where certain objects are **affected by side
effects** (such as setting options or controlling random number
generation) from previous calls, this dependency can be specified in the
`qenv`. You achieve this by adding the comment `# @linksto` followed by
the name of the linked object.

`q_linked`` ``<-`` `[`qenv`](https://insightsengineering.github.io/teal.code/reference/qenv.md)`(``)`` ``q_linked`` ``<-`` `[`eval_code`](https://insightsengineering.github.io/teal.code/reference/eval_code.md)`(``q_reproducible``, ``"`` `` set.seed(2) # @linksto a`` `` a <- runif(1)`` ``"``)`` `[`cat`](https://rdrr.io/r/base/cat.html)`(`[`get_code`](https://insightsengineering.github.io/teal.code/reference/get_code.md)`(``q_linked``)``)`

    ## a <- 2
    ## b <- 5
    ## c <- a + b
    ## 
    ##   set.seed(2) # @linksto a
    ##   a <- runif(1)

[`cat`](https://rdrr.io/r/base/cat.html)`(`[`get_code`](https://insightsengineering.github.io/teal.code/reference/get_code.md)`(``q_linked``, names ``=`` ``"a"``)``)`

    ## a <- 2
    ##   set.seed(2) # @linksto a
    ##   a <- runif(1)

Currently, object linking for reproducibility is only supported by the
`eval_code` function. Since `within` uses an expression as input and
ignores comments, its use is **not recommended** when side-effect
functions are required for full reproducibility.

## `qenv` and `teal` applications

The versatile `qenv` object can seamlessly integrate into teal modules.
Explore the teal vignette [Creating Custom
Modules](https://insightsengineering.github.io/teal/latest-tag/articles/creating-custom-modules.html)
for detailed guidance.
