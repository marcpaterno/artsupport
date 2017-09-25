#----------------------------------------------------------------------------
# Functions for filtering data frames, exported for the user.
#----------------------------------------------------------------------------


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
      dplyr::top_n(n, t) %>%
      getElement("ModuleLabel") %>%
      unique
  } else {
    data %>%
      dplyr::group_by(.data$ModuleLabel) %>%
      dplyr::summarize(t = median(.data$Time)) %>%
      dplyr::top_n(n, t) %>%
      getElement("ModuleLabel") %>%
      unique
  }
  data[data$ModuleLabel %in% wanted_labels, ]
}
