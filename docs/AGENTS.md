# AGENTS.md

This file provides guidance to Codex (Codex.ai/code) when working with
code in this repository.

## Package Overview

**BioticExplorerServer** is an R package that downloads NMD Biotic XML
data from the Institute of Marine Research (IMR) internal API and
compiles it into a DuckDB database. It serves as the backend data engine
for the BioticExplorer Shiny application. All core functionality
requires IMR intranet access (VPN or HI-adm WiFi).

## Development Commands

``` r

# Generate documentation from Roxygen2 comments and update NAMESPACE
devtools::document()

# Build and check the package (runs R CMD check)
devtools::check()

# Load all functions into the current session (faster than install)
devtools::load_all()

# Install the package
devtools::install()
```

There are no automated tests. Manual testing requires IMR intranet
access.

## Architecture

### Data Flow

    IMR NMD API (XML)
           │
           ▼
    downloadDatabase()      ← called per year inside compileDatabase()
           │
           ▼
    downloadDatabase()      ← writes taxaindex prepared by prepareTaxaList()
           │
           ▼
    bioticToDatabase()      ← parses XML via RstoxData::readXmlFile(), merges
           │                   mission/station/catchsample/individual/age tables,
           │                   adds ICES area (spatial join via sf),
           │                   adds gear category (lookup via gearCodes)
           ▼
    DuckDB tables: mission, stnall, indall, ageall, filesize, metadata,
                   source_manifest, csindex, gearindex, taxaindex
           │
           ▼
    indexDatabase()         ← creates dbIndex.rda for BioticExplorer Shiny app

### Main Entry Point

[`compileDatabase()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/compileDatabase.md)
orchestrates the full workflow. It calls
[`prepareCruiseSeriesList()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareCruiseSeriesList.md),
[`prepareGearList()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareGearList.md),
and
[`prepareTaxaList()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareTaxaList.md)
for reference data, then loops over years calling
[`downloadDatabase()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/downloadDatabase.md),
and finally calls
[`indexDatabase()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/indexDatabase.md).
The database typically takes several hours to compile from scratch and
requires \>2 GB of disk space.

[`updateDatabase()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/updateDatabase.md)
performs routine updates. It builds a metadata-only delivery manifest
from the Biotic API and transactionally replaces only changed years. If
the stored database schema version differs from the package schema
version, it calls
[`compileDatabase()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/compileDatabase.md)
to build and validate a complete sibling database before swapping it
into place. Any future change to fact table structure or derived-data
semantics must increment `.BES_DATABASE_SCHEMA_VERSION`.

### Key Design Points

- **DuckDB connection**: Only one write connection is allowed at a time;
  read-only connections can coexist. The connection is passed explicitly
  between functions (not stored globally).
- **S3 class**:
  [`bioticToDatabase()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/bioticToDatabase.md)
  returns a `bioticProcData` object with a custom `print` method. The
  main tables are `$stnall`, `$indall`, `$ageall`, `$mission`.
- **Reference tables**: `csindex`, `gearindex`, and `taxaindex` are
  written to DuckDB.
  [`compileDatabase()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/compileDatabase.md)
  writes `csindex` and `gearindex`;
  [`downloadDatabase()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/downloadDatabase.md)
  writes `taxaindex` and also ensures it exists for callers that bypass
  [`compileDatabase()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/compileDatabase.md).
- **Bundled reference data**: `data/cruiseSeries.rda`,
  `data/gearList.rda`, and `data/icesAreas.rda` are pre-compiled and
  loaded automatically. They are updated by running
  [`prepareCruiseSeriesList()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareCruiseSeriesList.md),
  [`prepareGearList()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareGearList.md),
  and
  [`prepareICESareas()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareICESareas.md)
  respectively, then saved with
  `usethis::use_data(..., overwrite = TRUE)`.
- **Network retries**:
  [`downloadDatabase()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/downloadDatabase.md)
  retries failed downloads up to 4 times before giving up on a year.
- **Timeout**: `zzz.R` sets `options(timeout = 3000)` on package attach
  to handle large XML downloads.
- **Privacy**: Do not commit generated docs or examples that include
  absolute local paths, usernames, database locations, credentials, or
  other machine-specific output.

### API Endpoints

- Biotic data:
  `https://biotic-api.hi.no/apis/nmdapi/biotic/v3/{year}/cache?version=3.1`
- Reference data:
  `https://reference-api.hi.no/apis/nmdapi/reference/v2/...`

### Database Usage (Read-Only, e.g. from BioticExplorer)

``` r

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "path/to/db.duckdb", read_only = TRUE)
stnall  <- dplyr::tbl(con, "stnall")   # station-level data (lazy)
indall  <- dplyr::tbl(con, "indall")   # individual fish measurements (lazy)
ageall  <- dplyr::tbl(con, "ageall")   # age readings (lazy)
mission <- dplyr::tbl(con, "mission")  # cruise/mission metadata (lazy)
taxaindex <- dplyr::tbl(con, "taxaindex") # taxa reference data (lazy)
```
