# Download and parse NMD Biotic data to files

Downloads annual NMD Biotic data from the API and saves them as XML
and/or RDS files on disk. Unlike
[`downloadDatabase`](https://deepwaterimr.github.io/BioticExplorerServer/reference/downloadDatabase.md),
this function does not write to a DuckDB database.

## Usage

``` r
downloadDatabaseToFiles(
  years,
  dest,
  method = "compare",
  save = c("xml", "rds"),
  icesAreas = NULL,
  cruiseSeries = NULL,
  gearCodes = NULL
)
```

## Arguments

- years:

  Vector of integers specifying the years to be downloaded. The database
  reaches 1914:year(Sys.Date())

- dest:

  Character string specifying the folder path where downloaded files
  should be saved. Subdirectories `XMLfiles/` and `Rdata/` are created
  automatically.

- method:

  Character string controlling what to do when a file already exists.
  `"compare"` (default) re-downloads and overwrites; `"keep"` skips
  existing files.

- save:

  Character vector of output formats. Can include `"xml"` (raw XML,
  always saved) and/or `"rds"` (parsed
  [`bioticToDatabase`](https://deepwaterimr.github.io/BioticExplorerServer/reference/bioticToDatabase.md)
  output as RDS).

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

Called for its side effects: saves XML and/or RDS files to `dest`.
Returns `NULL` invisibly.

## Author

Ibrahim Umar, Mikko Vihtakari (Institute of Marine Research)
