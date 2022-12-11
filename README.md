# teal.code

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering//actions/workflows/check.yaml/badge.svg)](https://github.com/insightsengineering//actions/workflows/check.yaml)
[![Docs 📚](https://github.com/insightsengineering//actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io//)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering//_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering//_xml_coverage_reports/data/main/coverage.xml)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/?style=social)
![GitHub Repo stars](https://img.shields.io/github/stars/insightsengineering/?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering//main?color=purple\&label=package%20version)](https://github.com/insightsengineering//tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/?color=red\&label=open%20issues)](https://github.com/insightsengineering//issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

# teal.code

<!-- start badges -->
[![Code Coverage](https://raw.githubusercontent.com/insightsengineering/teal.code/_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering/teal.code/_xml_coverage_reports/data/main/coverage.xml)
<!-- end badges -->

## Overview

`teal.code` is an R library providing tools to store code and an execution environment associated with it. The features
include:

* an object `qenv` for storing code and an execution environment which integrates well with `shiny reactives` for use in `shiny` applications whose outputs require reproducibility (i.e. the code used to generate them)
* ability to chain and join `qenv` objects together to provide fine-grained control over executed code
* automatic error and warning handling for executed code

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
