#----------------------------------------------------------------------------
# Plotting functions exported for the user.
#----------------------------------------------------------------------------


#' Make a box-and-whisker plot of module times from a timing dataframe.
#'
#' @param data The dataframe to plot, as from \code{load_module_timing}
#' @param n    If non-NULL, plot only the top \code{n} entries from each label
#' @param ... additional arguments passed to \code{geom_boxplot}
#' @param use_log if TRUE, a log scale is used for the $x$ axis
#'
#' @return a lattice object, as from bwplot
#' @export
#' @importFrom stats median
#' @importFrom magrittr "%>%"
#'
module_bwplot <- function(data, n = NULL, use_log = FALSE, ...) {
  checkmate::check_data_frame(data)
  if (!is.null(n))
    checkmate::check_count(n)
  # If n is set, subset the data to include only those with the largest
  # median Times.
  interesting <- if (is.null(n))
    data
  else
    get_interesting(data, n)

  p <- ggplot2::ggplot(interesting,
                       ggplot2::aes(.data$Time,
                                    stats::reorder(.data$ModuleLabel, .data$Time))) +
    ggplot2::geom_boxplot(...) +
    ggplot2::labs(x = "time (s)", y = "Module label")
  if (isTRUE(use_log))
    p <- p + ggplot2::scale_x_log10()
  p
}
