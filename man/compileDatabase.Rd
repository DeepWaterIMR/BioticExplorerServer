% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compileDatabase.R
\name{compileDatabase}
\alias{compileDatabase}
\title{Download IMR Biotic database and to place it into a \link[duckdb]{duckdb} database}
\usage{
compileDatabase(
  years = 1900:data.table::year(Sys.time()),
  dbPath = "~/IMR_biotic_BES_database",
  dbIndexFile = file.path(dbPath, "dbIndex.rda"),
  dbName = NULL,
  overwrite = FALSE
)
}
\arguments{
\item{years}{Vector of integers specifying the years to be downloaded. The database reaches 1914:year(Sys.Date())}

\item{dbPath}{Character string specifying the folder where the \link[duckdb]{duckdb} and \link[=indexDatabase]{dbIndex} files should be saved.}

\item{dbIndexFile}{Character string specifying the file path where the index of the database should be saved. Must include \code{.rda} at the end. The index is used by \href{https://github.com/DeepWaterIMR/BioticExplorer}{BioticExplorer}.}

\item{dbName}{Character string or \code{NULL}. If \code{NULL} uses the default names ("bioticexploer").}

\item{overwrite}{Logical indicating whether existing information in the \link[duckdb]{duckdb} database (\code{dbPath}) should be downloaded again and overwritten.}
}
\description{
Downloads, formulates and indexes IMR Biotic database into a format used by BioticExplorer
}
\details{
Runs the \code{\link{prepareCruiseSeriesList}}, \code{\link{prepareGearList}}, \code{\link{downloadDatabase}} and \code{\link{indexDatabase}} functions, and saves the results into a \link[duckdb]{duckdb}. Be aware that running these functions requires access to the IMR intranet and reasonably stable internet. It is advisable to run the function in a separate R session or in a screen session in the terminal on Unix machines, as downloading the database takes several hours and requires a stable internet connection. If the connection is unstable, the function may return an error. In such cases, ensure that the connection is stable and rerun the function. The function should continue downloading from where it left off.
}
\author{
Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research)
}
