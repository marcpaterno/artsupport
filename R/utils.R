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
  tibble::add_column(tmp, sample = 1:nr)
}

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

#----------------------------------------------------------------------------
# Functions exported for the user
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

#' Filter a module timing data to include only those with largest median run time.
#'
#' @param data a tibble, as returned by load_module_timing
#' @param n    the number of modules labels to return. If there are ties in the
#'              data, more will be returned.
#'
#' @return a dataframe
#' @export
#'
get_interesting <- function(data, n) {
  # TODO: Figure out how to remove the duplication in this code.
  wanted_labels <- if ("lbl" %in% names(data)) {
    data %>%
      dplyr::group_by(.data$lbl, .data$ModuleLabel) %>%
      dplyr::summarize(t = median(.data$Time)) %>%
      dplyr::top_n(n) %>%
      getElement("ModuleLabel") %>%
      unique
  } else {
    data %>%
      dplyr::group_by(.data$ModuleLabel) %>%
      dplyr::summarize(t = median(.data$Time)) %>%
      dplyr::top_n(n) %>%
      getElement("ModuleLabel") %>%
      unique
  }
  data[data$ModuleLabel %in% wanted_labels, ]
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

#' Make a box-and-whisker plot of module times from a timing dataframe.
#'
#' @param data The dataframe to plot, as from \code{load_module_timing}
#' @param maxmodules If non-NULL, plot only the top maxmodules entries from each label
#' @param ... additional arguments passed to \code{bwplot}
#'
#' @return a lattice object, as from bwplot
#' @export
#' @importFrom stats median
#' @importFrom magrittr "%>%"
#'
module_bwplot <- function(data, maxmodules = NA, ...) {
  checkmate::check_data_frame(data)
  checkmate::check_count(maxmodules, na.ok = TRUE)
  # If maxmodules is set, subset the data to include only those with the largest
  # median Times.
  interesting <- if (is.na(maxmodules)) data else get_interesting(data, maxmodules)

  lattice::bwplot(
    stats::reorder(ModuleLabel, Time) ~ Time,
    data = interesting,
    scales = list(x = list(log = 10, equispaced = FALSE)),
    ...
  )
}
