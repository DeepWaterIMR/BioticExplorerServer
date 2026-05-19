# Read and process a NMD Biotic xml file for further use in the BioticExplorer database

A wrapper for
[`readXmlFile`](https://rdrr.io/pkg/RstoxData/man/readXmlFile.html) to
enable further use in the BioticExplorer database

## Usage

``` r
bioticToDatabase(
  file,
  removeEmpty = FALSE,
  convertColumns = FALSE,
  returnOriginal = FALSE,
  missionidPrefix = NULL,
  icesAreas = NULL,
  cruiseSeries = NULL,
  gearCodes = NULL
)
```

## Arguments

- file:

  character string specifying the file path to the xml file. Accepts
  only one file at the time.

- removeEmpty:

  logical indicating whether empty columns should be removed from the
  output.

- convertColumns:

  logical indicating whether the column types should be converted. See
  `link{convertColumnTypes}`. Setting this to `FALSE` considerably
  speeds up the function, but leads to problems with non-unicode
  characters.

- returnOriginal:

  logical indicating whether the original data (`$mission` through
  `$agedetermination`) should be returned together with combined data.

- missionidPrefix:

  A prefix for the `missionid` identifier, which separates cruises. Used
  when several xml files are put together. `NULL` (default) omits the
  prefix.

- icesAreas:

  ICES area shape
  [`st_polygon`](https://r-spatial.github.io/sf/reference/st.html)
  abject. Used for calculating the ICES area for a specific fishstation.

- cruiseSeries:

  a data.table object of NMD cruise series list. Used to identify cruise
  series of a specific mission. See
  [`prepareCruiseSeriesList`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareCruiseSeriesList.md).

- gearCodes:

  a data.table object of NMD gear code list. Used to make gearname and
  gearcategory columns. See
  [`prepareGearList`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareGearList.md).

## Value

Returns a list of Biotic data with `$mission`, `$stnall` and `$indall`
data tables. The `$stnall` and `$indall` are merged from `$fishstation`
and `$catchsample` (former) and `$fishstation`, `$catchsample`,
`$individual` and `$agedetermination` (latter).

## Author

Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research)
