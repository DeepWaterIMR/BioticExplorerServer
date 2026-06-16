# Prepare coded-field reference list

Downloads and compiles the simple (non-composite) coded-field reference
tables from the IMR NMD Reference API into a single long-format lookup.
These are the Biotic columns flagged as codes of type `KeyType` (e.g.
`sex`, `maturationstage`, `missiontype`, `nation`) whose meaning is
otherwise only resolvable against the API. The result is written to the
DuckDB database as the `codeindex` table by
[`compileDatabase`](https://deepwaterimr.github.io/BioticExplorerServer/reference/compileDatabase.md),
so agents and the Shiny app can decode these fields offline with a join
instead of a per-code network call.

## Usage

``` r
prepareReferenceCodes(tables = NULL, lang = c("en", "no"))
```

## Arguments

- tables:

  Character vector of reference-table names to pull. Defaults to the
  coded `KeyType` columns that actually occur in
  `mission`/`stnall`/`indall`. Note that a few Biotic columns map to a
  differently named reference dataset (e.g. the `gear` column resolves
  against the `equipment` table — handled separately by
  [`prepareGearList`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareGearList.md)
  and therefore not included here).

- lang:

  Language for `shortname`/`description`: `"en"` (default) or `"no"`.

## Value

A [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
with columns `reftable`, `code`, `shortname`, and `description`, stacked
across all successfully read tables.

## Details

Reads each table from `.../reference/v2/dataset/{table}` (the same
endpoint family used by
[`prepareGearList`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareGearList.md))
and keeps only the `code -> meaning` mapping. Editor-identity columns
from the registry (`updatedBy`, `insertedBy`, timestamps, `...By`
fields) are dropped on purpose so no staff usernames land in the
database. Tables that fail to download (e.g. off the IMR network) are
skipped with a warning rather than aborting the build. Composite
reference tables that are keyed by taxa and/or sex (`specialstage`,
`eggstage`, `moultingstage`, `spawningfrequency`) are intentionally
*not* handled here — they need a taxa/sex-aware lookup and are left to
the API.

## Author

Mikko Vihtakari (Institute of Marine Research)
