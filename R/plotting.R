#----------------------------------------------------------------------------
# Plotting functions exported for the user.
#----------------------------------------------------------------------------


#' Make a box-and-whisker plot of module times from a timing dataframe.
#'
#' @param data The dataframe to plot, as from \code{load_module_timing}
#' @param n    If non-NULL, plot only the top \code{n} entries from each label
#' @param ... additional arguments passed to \code{bwplot}
#'
#' @return a lattice object, as from bwplot
#' @export
#' @importFrom stats median
#' @importFrom magrittr "%>%"
#'
module_bwplot <- function(data, n = NA, ...) {
  checkmate::check_data_frame(data)
  checkmate::check_count(n, na.ok = TRUE)
  # If n is set, subset the data to include only those with the largest
  # median Times.
  interesting <- if (is.na(n)) data else get_interesting(data, n)

  lattice::bwplot(
    stats::reorder(ModuleLabel, Time) ~ Time,
    data = interesting,
    scales = list(x = list(log = 10, equispaced = FALSE)),
    ...
  )
}
