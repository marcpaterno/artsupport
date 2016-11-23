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
  dplyr::src_sqlite(filename) %>%
    dplyr::tbl("TimeModule") %>%
    dplyr::tbl_df %>%
    cbind(lbl)
}

#' Make a box-and-whisker plot of module times from a timing dataframe.
#'
#' @param data The dataframe to plot
#' @return a lattice object, as from bwplot
#' @export
module_bwplot <- function(data,  ...) {
  lattice::bwplot(
    reorder(ModuleLabel, Time) ~ Time | lbl,
    data = data,
    scales = list(x = list(log = 10, equispaced = FALSE)),
    ...
  )
}
