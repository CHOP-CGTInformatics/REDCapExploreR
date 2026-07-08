
<!-- README.md is generated from README.Rmd. Please edit that file -->

# REDCapExploreR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

The goal of REDCapExploreR is to provide users with tools to
post-process and perform exploratory data analysis on REDCap project
data through the REDCap API.

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
redcap_uri <- Sys.getenv("REDCAP_URI")
token <- Sys.getenv("REDCAP_TOKEN")

data <- get_record_status_data(redcap_uri = redcap_uri, token = token)

plot_record_status(data)
```

For larger projects, use compact mode to reduce x-axis text size, truncate long
form labels, and draw lighter tile borders.

``` r
plot_record_status(data, mode = "compact")

plot_record_status(data, mode = "compact", form_label_max = 20)
```

Because `plot_record_status()` returns a ggplot object, you can add standard
ggplot2 layers to customize the display.

``` r
plot_record_status(data) +
  ggplot2::labs(title = "REDCap Record Status")
```
