# teal.code

<!-- start badges -->

[![CRAN Version](https://www.r-pkg.org/badges/version/teal.code?color=green)](https://cran.r-project.org/package=teal.code)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/teal.code?color=green)](https://cran.r-project.org/package=teal.code)
[![Last Month Downloads](http://cranlogs.r-pkg.org/badges/last-month/teal.code?color=green)](https://cran.r-project.org/package=teal.code)
[![Last Week Downloads](http://cranlogs.r-pkg.org/badges/last-week/teal.code?color=green)](https://cran.r-project.org/package=teal.code)

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

`teal.code` is an R library providing tools to store code and an execution environment associated with it.
The features include:

* the `qenv` class for tracking code and storing variables that integrates well with `shiny` reactive expressions for use in `shiny` applications whose outputs require reproducibility (_i.e._ the code used in the application can be retrieved and rerun)
* ability to chain and join `qenv` objects together to provide fine-grained control over executed code
* automatic handling of errors and warnings encountered when executed code

## Installation

```r
# stable versions
install.packages('teal.code')

# install.packages("pak")
pak::pak("insightsengineering/teal.code@*release")
```

Alternatively, you might want to use the development version available on [r-universe](https://r-universe.dev/).

```r
# beta versions
install.packages('teal.code', repos = c('https://pharmaverse.r-universe.dev', getOption('repos')))

# install.packages("pak")
pak::pak("insightsengineering/teal.code")
```

## Usage

To understand how to use this package, please refer to the [Getting Started](https://insightsengineering.github.io/teal.code/latest-tag/articles/teal-code.html) article, which provides multiple examples of code implementation.

Below is the showcase of the example usage

```r
library(teal.code)
my_qenv <- qenv() |> eval_code("x <- 5")
my_qenv
#> <environment: 0x00000225cc85c7a0> [L]
#> Parent: <environment: package:teal.code>
#> Bindings:
#> • x: <dbl> [L]
get_env(my_qenv)
#> <environment: 0x00000225cc85c7a0>
ls(get_env(my_qenv))
#> [1] "x"
```

```r
qenv_2 <- eval_code(my_qenv, "y <- x * 2") |> eval_code("z <- y * 2")
qenv_2
#> <environment: 0x00000225ca866d68> [L]
#> Parent: <environment: package:teal.code>
#> Bindings:
#> • x: <dbl> [L]
#> • y: <dbl> [L]
#> • z: <dbl> [L]
get_env(qenv_2)
#> <environment: 0x00000225ca866d68>
ls(get_env(qenv_2))
#> [1] "x" "y" "z"
```

```r
qenv_2[["y"]]
#> [1] 10
```

```r
cat(get_code(qenv_2))
#> x <- 5
#> y <- x * 2
#> z <- y * 2
```

## Getting help

If you encounter a bug or have a feature request, please file an issue. For questions, discussions, and updates, use the `teal` channel in the [`pharmaverse` slack workspace](https://pharmaverse.slack.com).

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.code.svg)](https://starchart.cc/insightsengineering/teal.code)

### Stargazers

[![Stargazers repo roster for @insightsengineering/teal.code](http://reporoster.com/stars/insightsengineering/teal.code)](https://github.com/insightsengineering/teal.code/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/teal.code](http://reporoster.com/forks/insightsengineering/teal.code)](https://github.com/insightsengineering/teal.code/network/members)
