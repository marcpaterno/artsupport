#----------------------------------------------------------------------------
# Utility functions, not exported for the user.
#----------------------------------------------------------------------------

#' Names of tables in a _MemoryTracker_ database.
#'
#' @return A character vector containing the names of the tables in the MemoryTracker database.
#'
#' @examples
#'   library(artsupport)
#'   memory_db_tables()
#' @export
#'
memory_db_tables <- function() {
  c("ModuleInfo", "EventInfo", "OtherInfo", "PeakUsage")
}
