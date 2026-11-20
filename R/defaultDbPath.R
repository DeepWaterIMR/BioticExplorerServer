## Internal: kept as a function so that the Windows branch of defaultDbPath() can be
## exercised on other platforms in the tests.

.isWindows <- function() {
  identical(.Platform$OS.type, "windows")
}

## Internal: the user's home folder. On Windows %USERPROFILE%, because R's ~ expansion
## goes through the Documents special folder, which OneDrive may redirect.

.homeDir <- function() {
  if (.isWindows()) {
    profile <- Sys.getenv("USERPROFILE")

    if (nzchar(profile)) {
      return(normalizePath(profile, winslash = "/", mustWork = FALSE))
    }
  }

  path.expand("~")
}

## Internal: default dbPath for functions that work on an existing database
## (updateDatabase, repairDatabaseEncoding, indexDatabase). Uses the folder of a database
## found in any current or legacy location, so an unmigrated legacy install is updated in
## place instead of triggering a multi-hour compile in the new default folder.

.existingDbPath <- function(dbName = NULL) {
  found <- findDatabase(dbName = dbName)
  if (is.null(found)) return(defaultDbPath())
  dirname(found)
}

## Internal: join path parts, dropping a NULL or empty root.

.dbFolder <- function(home, root, folder) {
  if (is.null(root) || !nzchar(root)) return(file.path(home, folder))
  file.path(home, root, folder)
}

#' @title Default location of the IMR Biotic database
#' @description Returns the platform-appropriate default folder for the
#'   \link[duckdb]{duckdb} database compiled by \code{\link{compileDatabase}}.
#' @param folder Character string giving the name of the database folder. Defaults to
#'   \code{"IMR_biotic_BES_database"}.
#' @param root Character string giving the shared data folder inside the user's home
#'   directory that holds \code{folder}. Defaults to \code{"Fiskwise_data"}, the folder in
#'   which \href{https://github.com/DeepWaterIMR/fiskwise}{Fiskwise} keeps all its local
#'   databases. Use \code{NULL} or \code{""} to place \code{folder} directly in the home
#'   directory (the layout used before BioticExplorerServer 0.8.14).
#' @details On macOS and Linux the default is
#'   \code{~/Fiskwise_data/IMR_biotic_BES_database}. On Windows, R expands \code{~} through
#'   the "personal"/Documents special folder, which OneDrive's Known Folder Move frequently
#'   redirects into a synchronized \code{OneDrive - <Organization>\\Documents} folder. A
#'   multi-gigabyte DuckDB file does not belong in a cloud-synchronized folder, so the
#'   default resolves to \code{\%USERPROFILE\%\\Fiskwise_data\\IMR_biotic_BES_database} on
#'   Windows. If \code{USERPROFILE} is not set, the function falls back to \code{~}
#'   expansion.
#' @return Character string giving an absolute path to the default database folder. The
#'   folder is not created.
#' @seealso \code{\link{dbPathCandidates}} and \code{\link{findDatabase}} for locating a
#'   database installed to one of the earlier default locations.
#' @section AI assistance:
#' Written with Claude Opus 5.5 (Anthropic); reviewed in full by Claude Opus 5.5
#' (Anthropic), 2026-11-20.
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @examples
#' defaultDbPath()
#' defaultDbPath(root = NULL)
#' @export

defaultDbPath <- function(folder = "IMR_biotic_BES_database", root = "Fiskwise_data") {
  .dbFolder(.homeDir(), root, folder)
}

#' @title Plausible locations of the IMR Biotic database
#' @description Returns the folders in which an IMR Biotic database may have been
#'   installed, most likely first.
#' @inheritParams defaultDbPath
#' @details The current default (\code{\link{defaultDbPath}}, inside \code{root}) comes
#'   first, followed by the legacy location directly in the home directory, which
#'   databases compiled before BioticExplorerServer 0.8.14 use. On Windows each of these is
#'   listed twice: under \code{\%USERPROFILE\%} and under the location that raw \code{~}
#'   expansion produces (typically the Documents folder, which may be redirected into
#'   OneDrive and was used before version 0.8.7).
#' @return Character vector of unique absolute paths. The folders are not created and are
#'   not guaranteed to exist.
#' @seealso \code{\link{defaultDbPath}}, \code{\link{findDatabase}}
#' @section AI assistance:
#' Written with Claude Opus 5.5 (Anthropic); reviewed in full by Claude Opus 5.5
#' (Anthropic), 2026-11-20.
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @examples
#' dbPathCandidates()
#' @export

dbPathCandidates <- function(folder = "IMR_biotic_BES_database", root = "Fiskwise_data") {
  homes <- c(.homeDir(), path.expand("~"))

  unique(c(
    .dbFolder(homes, root, folder),
    .dbFolder(homes, NULL, folder)
  ))
}

#' @title Locate an existing IMR Biotic database
#' @description Searches the plausible database locations and returns the path to the
#'   first \link[duckdb]{duckdb} file that exists.
#' @param dbName Character string or \code{NULL} giving the name of the database file
#'   without the \code{.duckdb} extension. If \code{NULL} uses the default name
#'   ("bioticexplorer").
#' @inheritParams defaultDbPath
#' @details Checks \code{\link{dbPathCandidates}} in order, so a database installed to the
#'   current default takes precedence over one left in a legacy location. Intended for
#'   downstream code, such as
#'   \href{https://github.com/DeepWaterIMR/BioticExplorer}{BioticExplorer}, which has to
#'   find a database it did not compile itself.
#' @return Character string giving the absolute path to an existing \code{.duckdb} file,
#'   or \code{NULL} if no database was found.
#' @seealso \code{\link{defaultDbPath}}, \code{\link{dbPathCandidates}}
#' @section AI assistance:
#' Written with Claude Opus 5.5 (Anthropic); reviewed in full by Claude Opus 5.5
#' (Anthropic), 2026-11-20.
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @examples
#' findDatabase()
#' @export

findDatabase <- function(dbName = NULL, folder = "IMR_biotic_BES_database",
                         root = "Fiskwise_data") {
  if (is.null(dbName)) dbName <- "bioticexplorer"

  candidates <- file.path(
    dbPathCandidates(folder = folder, root = root), paste0(dbName, ".duckdb")
  )

  found <- candidates[file.exists(candidates)]

  if (length(found) == 0) return(NULL)

  normalizePath(found[1], winslash = "/", mustWork = FALSE)
}
