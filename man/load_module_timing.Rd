% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loading_data.R
\name{load_module_timing}
\alias{load_module_timing}
\title{Functions for loading data.
Load module timing information from a \emph{TimeTracker} database}
\usage{
load_module_timing(filename, lbl = NULL, include_source = FALSE)
}
\arguments{
\item{filename}{The name of the \emph{TimeTracker} database file to open.}

\item{lbl}{The label to be applied to each row in the dataframe. Default is  NULL, in which cade no label is added.}

\item{include_source}{if TRUE, the timing for the source module is included}
}
\value{
a dataframe
}
\description{
Open the specified \emph{TimeTracker} database file, and read the
module-by-module, event-by-event timing information. If \code{lbl} is
non-NULL, the returned dataframe will contain a column named \code{lbl},
containing the user-supplied label. The intent is that this label allows many
such dataframes, each with a distinct label, to be concatenated with
\code{dplyr::rbind_rows} or \code{rbind}, and used for comparitive analysis.
}
\examples{
\dontrun{
d1 <- load_module_timing("woof/timing.db", "woof")
d2 <- load_module_timing("lsd6/timing.db", "lsd6")
d3 = rbind(d1, d2)
}
}
