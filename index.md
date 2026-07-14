# REDCapExploreR

The goal of REDCapExploreR is to provide users with tools to perform
exploratory data analysis on and quality assessments of REDCap project
data using the REDCap API.

This repository is in early stages active development!

## Installation

You can install the development version of REDCapExploreR like so:

``` r

devtools::install_github("CHOP-CGTInformatics/REDCapExploreR")
```

## Core workflows

Store REDCap credentials outside your scripts, then pass them to the
package’s build functions:

``` r

redcap_uri <- Sys.getenv("REDCAP_URI")
token <- Sys.getenv("REDCAP_TOKEN")

status_data <- build_record_status_data(redcap_uri, token)
plot_record_status(status_data)

codebook <- build_codebook(redcap_uri, token)
quality_report <- build_quality_report(redcap_uri, token)
```

The package includes `mock_record_status_data`, `mock_codebook`, and
`mock_quality_report` for offline exploration. See the getting-started
vignette for an overview and the [quality report
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
