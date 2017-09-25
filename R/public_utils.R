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

#' Count the distinct events in a data.frame.
#'
#' This function expects the data.frame to contain columns named 'Run',
#' 'SubRun', and 'Event', carrying the appropriate integer values. Events with
#' different labels (if the data.frame is labeled) are counted as distinct.
#'
#' @param data a data.frame
#'
#' @return the number of distinct events represented in the data.frame
#' @export
#'
count_events <- function(data) {
  checkmate::check_subset(data, c("Run", "SubRun", "Event"))
  if ("lbl" %in% names(data))
    nrow(distinct(data, .data$Run, .data$SubRun, .data$Event, .data$lbl))
  else
    nrow(distinct(data, .data$Run, .data$SubRun, .data$Event))
}
