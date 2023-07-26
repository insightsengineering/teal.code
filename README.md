# teal.code

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering/teal.code/actions/workflows/check.yaml/badge.svg)](https://insightsengineering.github.io/teal.code/main/unit-test-report/)
[![Docs 📚](https://github.com/insightsengineering/teal.code/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/teal.code/)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering/teal.code/_xml_coverage_reports/data/main/badge.svg)](https://insightsengineering.github.io/teal.code/main/coverage-report/)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/teal.code?style=social)
![GitHub repo stars](https://img.shields.io/github/stars/insightsengineering/teal.code?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/teal.code)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/teal.code)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/teal.code)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/teal.code)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/teal.code)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/teal.code)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering/teal.code/main?color=purple\&label=package%20version)](https://github.com/insightsengineering/teal.code/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/teal.code?color=red\&label=open%20issues)](https://github.com/insightsengineering/teal.code/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

## Overview

`teal.code` is an R library providing tools to store code and an execution environment associated with it. The features
include:

* an object `qenv` for storing code and an execution environment which integrates well with `shiny reactives` for use in `shiny` applications whose outputs require reproducibility (i.e. the code used to generate them)
* ability to chain and join `qenv` objects together to provide fine-grained control over executed code
* automatic error and warning handling for executed code

`teal.code` also ships a [`shiny`](https://shiny.rstudio.com/) module that helps inspect the stored code as well as messages, warnings and error messages resulting from evaluation via `shiny` web application.

## Installation

From July 2023 `insightsengineering` packages are available on [r-universe](https://r-universe.dev/).

```r
# stable versions
install.packages('teal.code', repos = c('https://insightsengineering.r-universe.dev', 'https://cloud.r-project.org'))

# beta versions
install.packages('teal.code', repos = c('https://pharmaverse.r-universe.dev', 'https://cloud.r-project.org'))
```

See package vignettes `browseVignettes(package = "teal.code")` for usage of this package.

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.code.svg)](https://starchart.cc/insightsengineering/teal.code)

### Stargazers

[![Stargazers repo roster for @insightsengineering/teal.code](https://reporoster.com/stars/insightsengineering/teal.code)](https://github.com/insightsengineering/teal.code/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/teal.code](https://reporoster.com/forks/insightsengineering/teal.code)](https://github.com/insightsengineering/teal.code/network/members)
