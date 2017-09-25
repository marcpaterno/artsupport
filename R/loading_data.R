#----------------------------------------------------------------------------
# Functions for loading data, exported for the user.
#----------------------------------------------------------------------------


#' Load module timing information from a \emph{TimeTracker} database
#'
#' Open the specified \emph{TimeTracker} database file, and read the
#' module-by-module, event-by-event timing information. If \code{lbl} is
#' non-NULL, the returned dataframe will contain a column named \code{lbl},
#' containing the user-supplied label. The intent is that this label allows many
#' such dataframes, each with a distinct label, to be concatenated with
#' \code{dplyr::rbind_list} or \code{rbind}, and used for comparitive analysis.
#'
#' @param filename The name of the \emph{TimeTracker} database file to open.
#' @param lbl The label to be applied to each row in the dataframe. Default is
#'   NULL, in which cade no label is added.
#' @return a dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' d1 <- load_module_timing("woof/timing.db", "woof")
#' d2 <- load_module_timing("lsd6/timing.db", "lsd6")
#' d3 = rbind(d1, d2)
#' }
load_module_timing <- function(filename, lbl = NULL) {
  load_table(filename, "TimeModule", lbl)
}

#' Load memory use information from a \emph{MemoryTracker} database
#'
#' Open the specified \emph{MemoryTracker} database file, and read the specified
#' table name info a dataframe. If \code{lbl} is non-NULL, the returned
#' dataframe will contain a column named \code{lbl}, containing the
#' user-supplied label.  The intent is that this label allows many such
#' dataframes, each with a distinct label, to be concatenated with
#' \code{dplyr::rbind_list} or \code{rbind}, and used for comparitive analysis.
#' Consult the \emph{MemoryTracker} documentation for a list of the tables that
#' are available.
#'
#' @param filename The name of the \emph{MemoryTracker} data file to open.
#' @param tablename The name of the table to be read.
#' @param lbl The label to be applied to each row in the dataframe; default is
#'   NULL.
#' @return a dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' d1 <- load_memory_use("woof/memory.db", "ModuleInfo", "woof")
#' d2 <- load_memory_use("woof/memory.db", "EventInfo")
#' d3 <- load_memory_use("woof/memory.db", "OtherInfo")
#' d4 <- load_memory_use("woof/memory.db", "PeakUsage")
#' }
load_memory_use <- function(filename, tablename, lbl = NULL) {
  checkmate::assert_choice(tablename, memory_db_tables())
  load_table(filename, tablename, lbl)
}

#' Load event-by-event memory use information from a MemoryTracker database
#'
#' @param filename The name of the \emph{MemoryTracker} data file top open.
#' @param lbl  The label to be applied to each row in the dataframe;default is NULL.
#'
#' @return a dataframe, with one row per event
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{
#' events <- load_event_memory_use("memory.db")
#' }
load_event_memory_use <- function(filename, lbl = NULL) {
  tmp <- load_memory_use(filename, "EventInfo", lbl)
  vsz <- tmp %>%
    dplyr::select(-c(.data$RSS, .data$sample)) %>%
    tidyr::spread(.data$Step, .data$Vsize) %>%
    dplyr::mutate(DeltaVsize =
                    .data$PostProcessEvent - .data$PreProcessEvent) %>%
    dplyr::rename(PreVsize =
                    .data$PreProcessEvent, PostVsize = .data$PostProcessEvent)
  rss <- tmp %>%
    dplyr::select(-c(.data$Vsize, .data$sample)) %>%
    tidyr::spread(.data$Step, .data$RSS) %>%
    dplyr::mutate(DeltaRSS = .data$PostProcessEvent - .data$PreProcessEvent) %>%
    dplyr::rename(PreRSS = .data$PreProcessEvent, PostRSS = .data$PostProcessEvent)
  tmp <- dplyr::inner_join(rss, vsz)
  nr <- nrow(tmp)
  tmp %>% dplyr::mutate(sample = 1:nr)
}
