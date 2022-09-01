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

For releases from August 2022 it is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/teal.code@*release")
```

A stable release of all `NEST` packages from June 2022 is also available [here](https://github.com/insightsengineering/depository#readme).

You might need to manually install all of the package dependencies before installing this package as without
the `dependencies = FALSE` argument to `install_github` it may produce an error.

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.code.svg)](https://starchart.cc/insightsengineering/teal.code)

### Stargazers

[![Stargazers repo roster for @insightsengineering/teal.code](https://reporoster.com/stars/insightsengineering/teal.code)](https://github.com/insightsengineering/teal.code/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/teal.code](https://reporoster.com/forks/insightsengineering/teal.code)](https://github.com/insightsengineering/teal.code/network/members)
