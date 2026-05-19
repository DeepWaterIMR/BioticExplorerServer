# Prepare gear list

Downloads and compiles the gear code reference list from the IMR API.
The result is bundled as
[`gearList`](https://deepwaterimr.github.io/BioticExplorerServer/reference/gearList.md)
package data. Run this function and save with
`usethis::use_data(gearList, overwrite = TRUE)` to refresh the bundled
data.

## Usage

``` r
prepareGearList()
```

## Value

A [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
with columns `code`, `gearname`, `gearcategory`, and `description`.

## Author

Mikko Vihtakari, Ibrahim Umar
