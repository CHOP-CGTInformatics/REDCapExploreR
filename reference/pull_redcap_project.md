# Pull REDCap project data for quality reporting

Retrieves records and project metadata from the REDCap API using
REDCapR.

## Usage

``` r
pull_redcap_project(redcap_uri, token)
```

## Arguments

- redcap_uri:

  REDCap API URI.

- token:

  REDCap API token.

## Value

A list containing raw records, metadata, events, event-instrument
mapping, instruments, and repeating instrument configuration.
