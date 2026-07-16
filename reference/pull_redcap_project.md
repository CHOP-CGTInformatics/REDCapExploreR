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
mapping, instruments, and repeating instrument configuration. Errors
from any structural metadata endpoint are reported rather than converted
into empty project structure. Records and headers use raw REDCap codes
with automatic type guessing disabled, and checkbox choices are returned
as raw `0`/`1` values. Only empty strings are imported as missing.
