# teal.code

## Overview

`teal.code` is an R library providing tools to store code and an execution environment associated with it. The features
include:
* storing character literals as code,
* storing an execution environment,
* swapping the execution environment of the stored code,
* evaluating only parts of the stored code,
* means to execute code with a no-throw guarantee (errors demoted to warnings and messages stored for retrieval).

`teal.code` also ships a [`shiny`](https://shiny.rstudio.com/) module that helps inspect the stored code as well as messages, warnings and error messages resulting from evaluation via `shiny` web application.

## Installation

This repository requires a personal access token to install see here [creating and using PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token). Once this is set up, to install the latest released version of the package run:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("devtools")) install.packages("devtools")
devtools::install_github("insightsengineering/teal.code@*release", dependencies = FALSE)
```

You might need to manually install all of the package dependencies before installing this package as without
the `dependencies = FALSE` argument to `install_github` it may produce an error.
