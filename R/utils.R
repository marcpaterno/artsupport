#' Load module timing information from a \emph{TimeTracker} database
#'
#' Open the specified \emph{TimeTracker} database file, and read the
#' module-by-module, event-by-event timing information. The returned dataframe
#' contains a column \code{lbl}, containing a user-supplied label. The intent is
#' that this label allows many such dataframes, each with a distinct label,
#' to be concatenated with \code{rbind}, and used for comparitive analysis.
#'
#' @param filename The name of the \emph{TimeTracker} database file to open.
#' @param lbl The label to be applied to each row in the dataframe.
#' @return a dataframe
#' @export
#'
#' @examples
#' d1 <- load_module_timing("woof/timing.db", "woof")
#' d2 <- load_module_timing("lsd6/timing.db", "lsd6")
#' d3 = rbind(d1, d2)
load_module_timing <- function(filename, lbl) {
  src_sqlite(filename) %>%
  tbl("TimeModule") %>%
  tbl_df %>%
  cbind(lbl)
}

#' Load memory use information from a \emph{MemoryTracker} database
#'
#' Open the specified \emph{MemoryTracker} database file, and read the
#' specified table name info a dataframe. The returned dataframe contains
#' a column \code{lbl}, containing a user-supplied label. The intent is that
#' this label allows many such dataframes, each with a distince label,
#' to be concatenated with \code{rbind}, and used for comparitive analysis.
#' Consult the \emph{MemoryTracker} documentation for a list of the tables
#' that are available.
#'
#' @param filename THe name of the \emph{MemoryTracker} data file to open.
#' @param tablename The name of the table to be read.
#' @param lbl The label to be applied to each row in the dataframe.
#' @return a dataframe
#' @export
#'
#' @examples
#' d1 <- load_memory_use("woof/memory.db", "ModuleInfo", "woof")
#' d2 <- load_memory_use("woof/memory.db", "EventInfo", "woof")
#' d3 <- load_memory_use("woof/memory.db", "OtherInfo", "woof")
#' d4 <- load_memory_use("woof/memory.db", "PeakUsage", "woof")
load_memory_use <- function(filename, tablename, lbl) {
  src_sqlite(filename) %>%
    tbl(tablename) %>%
    tbl_df %>%
    cbind(lbl)
}

#' Make a box-and-whisker plot of module times from a timing dataframe.
#'
#' @param data The dataframe to plot
#' @param maxmodules If non-NULL, plot only the top maxmodules entries from each label
#'
#' @return a lattice object, as from bwplot
#' @export
module_bwplot <- function(data, maxmodules = NULL, ...) {
  if (! is.null(maxmodules)) {
    interesting <- data %>% group_by(lbl,ModuleLabel) %>%
      summarize(t=median(Time)) %>%
      top_n(maxmodules) %>%
      getElement("ModuleLabel") %>%
      unique
    data <- subset(data, ModuleLabel %in% interesting)
  }
  lattice::bwplot(
    reorder(ModuleLabel, Time) ~ Time | lbl,
    data = data,
    scales = list(x = list(log = 10, equispaced = FALSE)),
    ...
  )
}
