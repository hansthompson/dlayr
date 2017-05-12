#' Returns a subset of the gtfs that is only the trips and service ids that are occuring for the day.
#'
#' @param url the link to the compressed gtfs files.
#' @param day defaults to Sys.Date(). Of class, Date.
#' @return an object of type gtfs from package gtfsr. Filters the gtfs object to just the current day.
daily_gtfs_obj <- function(url, day = Sys.Date()) {
  gtfs_obj <- import_gtfs(url)
  gtfs_drilldown(gtfs_obj, day)
}
