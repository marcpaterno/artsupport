#' Load moduline timing information from a _TimeTracker_ database
#'
#' Open the specified _TimeTracker_ database file, and read the
#' module-by-module, event-by-event timing information. The returned dataframe
#' contains a column "lbl", containing a user-supplied label. The intent is
#' that this label allows many such dataframes, each with a distinct label,
#' to be concatenated with `rbind`, and used for comparitive analysis.
#'
#' @param filename The name of the _TimeTracker_ database file to open.
#' @param lbl The label to be applied to each row in the dataframe.
#' @return a dataframe
#' @export
#'
#' @examples
#' d1 <- load_module_timing("woof/timing.db", "woof")
#' d2 <- load_module_timing("lsd6/timing.db", "lsd6")
#' d3 = rbind(d1, d2)
load_module_timing <- function(filename, lbl) {
  src_sqlite(filename) %>% tbl("TimeModule") %>% tbl_df %>% cbind(lbl)
}

#' Make a box-and-whisker plot of module times from a timing dataframe.
#'
#' @param data The dataframe to plot
#' @return a lattice object, as fron bwplot
#' @export
module_bwplot <- function(data, maxLables = NULL, ...) {
  bwplot(reorder(ModuleLabel, Time)~Time|lbl,
         data = data,
         scales = list(x = list( log = 10, equispaced = FALSE)),
         ...
  )
}

