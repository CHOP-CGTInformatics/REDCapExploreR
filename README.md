
<!-- README.md is generated from README.Rmd. Please edit that file -->

# REDCapExploreR <a href="https://chop-cgtinformatics.github.io/REDCapExploreR/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/CHOP-CGTInformatics/REDCapExploreR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CHOP-CGTInformatics/REDCapExploreR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/CHOP-CGTInformatics/REDCapExploreR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/CHOP-CGTInformatics/REDCapExploreR?branch=main)
<!-- badges: end -->

REDCapExploreR provides exploratory tools for REDCap projects accessed
through the REDCap API. Use it to build structured codebooks, review
form completion with record status dashboards, and generate general data
quality reports.

## Installation

You can install the development version of REDCapExploreR like so:

``` r
devtools::install_github("CHOP-CGTInformatics/REDCapExploreR")
```

## Core workflows

Store REDCap credentials outside your scripts, then pass them to the
build functions:

``` r
library(REDCapExploreR)

redcap_uri <- Sys.getenv("REDCAP_URI")
token <- Sys.getenv("REDCAP_TOKEN")

status_data <- build_record_status_data(
  redcap_uri = redcap_uri,
  token = token
)
plot_record_status(status_data)

codebook <- build_codebook(redcap_uri = redcap_uri, token = token)
quality_report <- build_quality_report(redcap_uri = redcap_uri, token = token)
```

The package includes `mock_redcap_project`, `mock_record_status_data`,
`mock_codebook`, and `mock_quality_report` for offline exploration. See
[Get
Started](https://chop-cgtinformatics.github.io/REDCapExploreR/articles/REDCapExploreR.html)
for an overview, the [function
reference](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/index.html)
for complete API documentation, and the [quality report
article](https://chop-cgtinformatics.github.io/REDCapExploreR/articles/quality-report.html)
for a comprehensive guide to quality report checks, output elements, and
assumptions.

## Collaboration

We invite you to give feedback and collaborate with us! If you are
familiar with GitHub and R packages, please feel free to submit a [pull
request](https://github.com/CHOP-CGTInformatics/REDCapExploreR/pulls).
Please do let us know if REDCapExploreR fails for whatever reason with
your database and submit a bug report by creating a GitHub
[issue](https://github.com/CHOP-CGTInformatics/REDCapExploreR/issues).

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/CHOP-CGTInformatics/REDCapExploreR/blob/main/CONDUCT.md).
By participating you agree to abide by its terms.
