#----------------------------------------------------------------------------
# Utility functions, not exported for the user.
#----------------------------------------------------------------------------

#' Load a table from an SQLite3 database
#'
#' Open the specified SQLite database file, and read the specified table name
#' info a dataframe. If \code{lbl} is non-NULL, the returned dataframe will
#' contain a column named \code{lbl}, containing the user-supplied label. The
#' intent is that this label allows many such dataframes, each with a distinct
#' label, to be concatenated with \code{rbind}, and used for comparitive
#' analysis. Consult the \emph{MemoryTracker} and \emph{TimeTracker}
#' documentation for a list of the tables that are available.
#'
#' @param filename The name of the SQLite3 database file to open.
#' @param tablename The name of the table to be read.
#' @param lbl Optional, a label to be applied to each row in the dataframe.
#' @return a dataframe
#'
#' @importFrom  magrittr "%>%"
#' @examples
#' \dontrun{
#' d1 <- load_table("woof-memory.db", "ModuleInfo", "woof")
#' d2 <- load_table("cluck-memory.db", "ModuleInfo", "cluck")
#' mods <- rbind(d1, d2)
#' }
load_table <- function(filename, tablename, lbl) {
  stopifnot(file.exists(filename))
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = filename)
  tmp <- dplyr::tbl(con, tablename) %>% tibble::as_tibble()
  DBI::dbDisconnect(con)
  if (!is.null(lbl))
    tmp <- tibble::add_column(tmp, lbl = lbl)
  nr <- nrow(tmp)
  # If there are no entries in the table, the addition of the 'sample' column is
  # different than in the normal case. The value chosen in this case is
  # arbitrary; is just has to be a 1d atomic vector.
  if (nr == 0) tibble::add_column(tmp, sample = 0)
  else         tibble::add_column(tmp, sample = 1:nr)
}
