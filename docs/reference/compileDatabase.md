# Download IMR Biotic database and to place it into a [duckdb](https://r.duckdb.org/reference/duckdb.html) database

Downloads, formulates and indexes IMR Biotic database into a format used
by BioticExplorer

## Usage

``` r
compileDatabase(
  years = 1900:data.table::year(Sys.time()),
  dbPath = "~/IMR_biotic_BES_database",
  dbIndexFile = file.path(dbPath, "dbIndex.rda"),
  dbName = NULL,
  overwrite = FALSE
)
```

## Arguments

- years:

  Vector of integers specifying the years to be downloaded. The database
  reaches 1914:year(Sys.Date())

- dbPath:

  Character string specifying the folder where the
  [duckdb](https://r.duckdb.org/reference/duckdb.html) and
  [dbIndex](https://deepwaterimr.github.io/BioticExplorerServer/reference/indexDatabase.md)
  files should be saved.

- dbIndexFile:

  Character string specifying the file path where the index of the
  database should be saved. Must include `.rda` at the end. The index is
  used by
  [BioticExplorer](https://github.com/DeepWaterIMR/BioticExplorer).

- dbName:

  Character string or `NULL`. If `NULL` uses the default names
  ("bioticexploer").

- overwrite:

  Logical indicating whether existing information in the
  [duckdb](https://r.duckdb.org/reference/duckdb.html) database
  (`dbPath`) should be downloaded again and overwritten.

## Value

Called for its side effects: creates and populates a DuckDB database and
index file. Returns `NULL` invisibly.

## Details

Runs the
[`prepareCruiseSeriesList`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareCruiseSeriesList.md),
[`prepareGearList`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareGearList.md),
[`downloadDatabase`](https://deepwaterimr.github.io/BioticExplorerServer/reference/downloadDatabase.md)
and
[`indexDatabase`](https://deepwaterimr.github.io/BioticExplorerServer/reference/indexDatabase.md)
functions, and saves the results into a
[duckdb](https://r.duckdb.org/reference/duckdb.html). Be aware that
running these functions requires access to the IMR intranet and
reasonably stable internet. It is advisable to run the function in a
separate R session or in a screen session in the terminal on Unix
machines, as downloading the database takes several hours and requires a
stable internet connection. If the connection is unstable, the function
may return an error. In such cases, ensure that the connection is stable
and rerun the function. The function should continue downloading from
where it left off.

## Author

Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research)
