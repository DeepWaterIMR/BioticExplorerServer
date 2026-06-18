# Incrementally update a BioticExplorer database

Uses metadata-only API requests to identify years containing changed,
added, or removed deliveries. Only those annual XML caches are
downloaded and transactionally replaced. If the database schema is
incompatible with this package version, a complete sibling database is
built with
[`compileDatabase`](https://deepwaterimr.github.io/BioticExplorerServer/reference/compileDatabase.md)
and safely swapped into place.

## Usage

``` r
updateDatabase(
  years = NULL,
  dbPath = "~/IMR_biotic_BES_database",
  dbIndexFile = file.path(dbPath, "dbIndex.rda"),
  dbName = NULL,
  verbose = TRUE
)
```

## Arguments

- years:

  Optional integer vector limiting metadata checks and incremental
  replacements. A schema compatibility rebuild always includes all
  years.

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

- verbose:

  Logical; emit delivery metadata-check progress messages.

## Value

Invisibly returns a list describing the update mode and changed years.
