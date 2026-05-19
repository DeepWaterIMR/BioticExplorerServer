# Prepare cruise series list

Downloads and compiles the cruise series reference list from the IMR
API. The result is bundled as
[`cruiseSeries`](https://deepwaterimr.github.io/BioticExplorerServer/reference/cruiseSeries.md)
package data. Run this function and save with
`usethis::use_data(cruiseSeries, overwrite = TRUE)` to refresh the
bundled data.

## Usage

``` r
prepareCruiseSeriesList()
```

## Value

A [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
with columns `cruise`, `platformname`, `startyear`, `cruiseseriescode`,
and `name`.

## Author

Ibrahim Umar, Mikko Vihtakari
