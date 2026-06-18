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

  Character string or `NULL`. If `NULL` uses the default name
  ("bioticexplorer").

- overwrite:

  Logical indicating whether requested years and reference tables
  already present in the
  [duckdb](https://r.duckdb.org/reference/duckdb.html) database should
  be downloaded again and replaced. Existing annual rows are deleted
  transactionally before replacement, so they are not duplicated.

## Value

Called for its side effects: creates and populates a DuckDB database and
index file. Returns `NULL` invisibly.

## Details

Runs the
[`prepareCruiseSeriesList`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareCruiseSeriesList.md),
[`prepareGearList`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareGearList.md),
[`prepareTaxaList`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareTaxaList.md),
[`prepareReferenceCodes`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareReferenceCodes.md),
[`downloadDatabase`](https://deepwaterimr.github.io/BioticExplorerServer/reference/downloadDatabase.md)
and
[`indexDatabase`](https://deepwaterimr.github.io/BioticExplorerServer/reference/indexDatabase.md)
functions, and saves the results into a
[duckdb](https://r.duckdb.org/reference/duckdb.html). The cruise-series,
gear and taxa reference lists are written as the `csindex`, `gearindex`
and `taxaindex` tables, respectively, and the coded `KeyType` fields
(`sex`, `maturationstage`, `missiontype`, `nation`, …) are written as
the long-format `codeindex` table so they can be decoded offline with a
join. Completed databases are stamped with the package and
database-schema versions used to build them;
[`updateDatabase`](https://deepwaterimr.github.io/BioticExplorerServer/reference/updateDatabase.md)
uses this information to decide whether an incremental update is safe.
Be aware that running these functions requires access to the IMR
intranet and reasonably stable internet. It is advisable to run the
function in a separate R session or in a screen session in the terminal
on Unix machines, as downloading the database takes several hours and
requires a stable internet connection. If the connection is unstable,
the function may return an error. In such cases, ensure that the
connection is stable and rerun the function. The function should
continue downloading from where it left off.

## Author

Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research)
