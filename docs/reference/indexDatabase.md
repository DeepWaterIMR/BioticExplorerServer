# Index BioticExplorer database

Loads BioticExplorer database and creates an index used by
BioticExplorer to save processing time.

## Usage

``` r
indexDatabase(
  connection,
  dbIndexFile = "~/IMR_biotic_BES_database/dbIndex.rda",
  fileOnly = TRUE
)
```

## Arguments

- connection:

  Object defining the
  [duckdb](https://r.duckdb.org/reference/duckdb.html) connection.
  Typically made within
  [`compileDatabase`](https://deepwaterimr.github.io/BioticExplorerServer/reference/compileDatabase.md).

- dbIndexFile:

  Character string specifying the file path where the index of the
  database should be saved. Must include `.rda` at the end. The index is
  used by
  [BioticExplorer](https://github.com/DeepWaterIMR/BioticExplorer).

- fileOnly:

  Logical indicating whether the result should only be saved to a file
  and not returned. If FALSE, no file is made and the result is returned
  instead.

## Value

If `fileOnly = TRUE` (the default), saves the index to `dbIndexFile` and
returns `NULL` invisibly. Otherwise returns a named list of pre-computed
index values used by the BioticExplorer Shiny app.

## Author

Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research)
