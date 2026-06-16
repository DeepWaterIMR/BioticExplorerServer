# Package index

## Package documentation

Quick access to the package documentation.

- [`BioticExplorerServer`](https://deepwaterimr.github.io/BioticExplorerServer/reference/BioticExplorerServer-package.md)
  [`BioticExplorerServer-package`](https://deepwaterimr.github.io/BioticExplorerServer/reference/BioticExplorerServer-package.md)
  : R Package for Downloading Server-Side Data for BioticExplorer

## Main workflow

Functions to download and compile IMR Biotic survey data into a DuckDB
database.
[`compileDatabase()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/compileDatabase.md)
is the main entry point and orchestrates the full workflow. The other
functions are called internally but can also be used independently. All
functions require IMR intranet access (VPN or HI-adm WiFi).

- [`compileDatabase()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/compileDatabase.md)
  :

  Download IMR Biotic database and to place it into a
  [duckdb](https://r.duckdb.org/reference/duckdb.html) database

- [`downloadDatabase()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/downloadDatabase.md)
  : Download and parse NMD data for the BioticExplorer database

- [`downloadDatabaseToFiles()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/downloadDatabaseToFiles.md)
  : Download and parse NMD Biotic data to files

- [`indexDatabase()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/indexDatabase.md)
  : Index BioticExplorer database

## Biotic data processing

Functions to parse a single NMD Biotic XML file into R data tables.
Called internally by the download functions but can be used
independently to inspect or process individual XML files.

- [`bioticToDatabase()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/bioticToDatabase.md)
  : Read and process a NMD Biotic xml file for further use in the
  BioticExplorer database

- [`print(`*`<bioticProcData>`*`)`](https://deepwaterimr.github.io/BioticExplorerServer/reference/print.bioticProcData.md)
  :

  Print processed NMD Biotic data (`bioticProcData`) objects

## Reference data preparation

Functions to download and refresh the bundled reference datasets from
the IMR API. Run these to update cruise series, gear codes, or taxa
lists, then save the result with
`usethis::use_data(..., overwrite = TRUE)`.

- [`prepareCruiseSeriesList()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareCruiseSeriesList.md)
  : Prepare cruise series list
- [`prepareGearList()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareGearList.md)
  : Prepare gear list
- [`prepareReferenceCodes()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareReferenceCodes.md)
  : Prepare coded-field reference list
- [`prepareTaxaList()`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareTaxaList.md)
  : Prepare taxa list

## Bundled datasets

Reference datasets bundled with the package. Updated by running the
corresponding `prepare*()` functions.

- [`cruiseSeries`](https://deepwaterimr.github.io/BioticExplorerServer/reference/cruiseSeries.md)
  : Cruise series list
- [`gearList`](https://deepwaterimr.github.io/BioticExplorerServer/reference/gearList.md)
  : Gear code list
- [`icesAreas`](https://deepwaterimr.github.io/BioticExplorerServer/reference/icesAreas.md)
  : ICES fishing areas
