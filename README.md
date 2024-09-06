
<!-- README.md is generated from README.Rmd. Please edit that file -->

# REDCapExploreR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

The goal of REDCapExploreR is to provide users with tools to
post-process and perform exploratory data analysis on REDCap project
data. It is intended to work hand in hand with the REDCapTidieR package.

This repository is in early stages active development!

## Installation

You can install the development version of REDCapExploreR like so:

``` r
devtools::install_github("CHOP-CGTInformatics/REDCapExploreR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(REDCapExploreR)
## basic example code
```

### Recreating a Project’s Record Status Dashboard

``` r
data <- record_status_dashboard(redcap_uri = Sys.getenv("REDCAP_URI"), token = token)
```
