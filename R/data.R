#' art event timing data for 240 events.
#'
#' Several datasets containing the data recorded by the *art* framework's Timing service.
#'

#' The event-by-event timing data written into the `TimeEvent` table, which contains one record per event.
#' @format A tibble with 240 observatoins of 5 variables:
#' \describe{
#'   \item{Run}{the run number}
#'   \item{SubRUn}{the subrun number}
#'   \item{Event}{the event number}
#'   \item{Time}{event processing time in seconds}
#'   \item{sample}{sample id, an integer unique for each event}
#' }
"events"

#' The module-by-module timing data written into the `TimeModule` table, which contains one record
#' per module for each event. The recorded art process used 3 modules on just 1 path. There is also
#' a record for the TriggerResults 'module'.
#' @format A tibble with 960 observatios of 8 variables:
#' \describe{
#'   \item{Run}{the run number}
#'   \item{SubRun}{the subrun number}
#'   \item{Event}{the event number}
#'   \item{Path}{the path on which the module ran; note that one module can appear on multiple paths}
#'   \item{ModuleLabel}{the label of the module}
#'   \item{ModuleType}{the C++ class of the module}
#'   \item{Time}{module executon time in seconds}
#'   \item{sample}{sample id, and integer unique for each event}
#' }
"modules"


