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

#' Load event timing information from a \emph{TimeTracker} database
#'
#' Open the specified \emph{TimeTracker} database file, and read the
#' event-by-event timing information. If \code{lbl} is
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
#' d1 <- load_event_timing("woof/timing.db", "woof")
#' d2 <- load_event_timing("lsd6/timing.db", "lsd6")
#' d3 = rbind(d1, d2)
#' }
load_event_timing <- function(filename, lbl = NULL) {
  load_table(filename, "TimeEvent", lbl)
}

#' Load source timing information from a \emph{TimeTracker} database
#'
#' Open the specified \emph{TimeTracker} database file, and read the
#' event-by-event timing information for the source. If \code{lbl} is
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
#' d1 <- load_source_timing("woof/timing.db", "woof")
#' d2 <- load_source_timing("lsd6/timing.db", "lsd6")
#' d3 = rbind(d1, d2)
#' }
load_source_timing <- function(filename, lbl = NULL) {
  load_table(filename, "TimeSource", lbl)
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

#' Load event-by-event module level memory use information
#'
#' @param filename The name of the \emph{MemoryTracker} data file top open.
#'
#' @return a dataframe, with one row per module per event
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{}
#' moduleMemUsage <- load_module_memory_use("memory.db")
#' }
load_module_memory_use <- function(filename) {
  tmp <- load_memory_use(filename, "ModuleInfo")
  # Collapse some of the columns
  tmpS <- tmp %>%
    dplyr::transmute(event=paste(padNum(Run), padNum(SubRun), padNum(Event), sep="/"),
              module=paste(stringr::str_remove(Step, '^(Pre|Post)'), Path, ModuleLabel, ModuleType, sep='/'),
              rss=RSS,
              prepost=paste0(stringr::str_extract(Step, "^(Pre|Post)"), "RSS")
  )

  # Check for consistiency
  preKeys =  tmpS %>% dplyr::filter(prepost=='PreRSS')  %>% dplyr::transmute(k=paste(event, module)) %>% dplyr::pull()
  postKeys = tmpS %>% dplyr::filter(prepost=='PostRSS') %>% dplyr::transmute(k=paste(event, module)) %>% dplyr::pull()
  all(preKeys == postKeys) %>% assertthat::assert_that(msg="Pre and Post rows do not alternate!")

  # Spread the dataframe pre/post and mark the runs that are root related
  tmpSpread <- tidyr::spread(tmpS, prepost, rss) %>% dplyr::mutate(deltaRSS=PostRSS-PreRSS,
                                                     isRootOut = stringr::str_detect(module, "RootOutput"))
  tmpSpread$sample <- 1:nrow(tmpSpread)

  tmpSpread
}
