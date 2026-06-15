# Download and parse NMD data for the BioticExplorer database

Downloads annual NMD data from the API and writes them as a DuckDB
database

## Usage

``` r
downloadDatabase(
  years,
  connection,
  icesAreas = NULL,
  cruiseSeries = NULL,
  gearCodes = NULL,
  taxaList = NULL,
  overwrite = FALSE
)
```

## Arguments

- years:

  Vector of integers specifying the years to be downloaded. The database
  reaches 1914:year(Sys.Date())

- connection:

  Object defining the
  [duckdb](https://r.duckdb.org/reference/duckdb.html) connection.
  Typically made within
  [`compileDatabase`](https://deepwaterimr.github.io/BioticExplorerServer/reference/compileDatabase.md).

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

- taxaList:

  a data.table object of NMD taxa reference data. Written to the
  `taxaindex` DuckDB table. See
  [`prepareTaxaList`](https://deepwaterimr.github.io/BioticExplorerServer/reference/prepareTaxaList.md).

- overwrite:

  Logical indicating whether existing information in the
  [duckdb](https://r.duckdb.org/reference/duckdb.html) database
  (`dbPath`) should be downloaded again and overwritten.

## Value

Called for its side effects: appends parsed Biotic data to the DuckDB
database. Returns `NULL` invisibly.

## Details

The function writes the taxa reference list to the database, downloads
NMD data from the API per year, saves these in temp files, reformats
them for the DuckDB and writes them into the database.

## Author

Ibrahim Umar, Mikko Vihtakari (Institute of Marine Research)
