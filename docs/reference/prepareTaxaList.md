# Prepare taxa list

Downloads and compiles the taxa reference list from the IMR API. The
result can be bundled as package data. Run this function and save with
`usethis::use_data(taxaList, overwrite = TRUE)` to refresh the bundled
data.

## Usage

``` r
prepareTaxaList(verbose = FALSE)
```

## Arguments

- verbose:

  Logical indicating whether the function should print progress
  messages.

## Value

A [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
with columns `tsn` and `name` (plus additional synonym columns where
available). Rows where `name` is `NA` are excluded.

## Author

Mikko Vihtakari, Ibrahim Umar
