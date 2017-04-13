#' Add together two numbers.
#' 
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
# Filter the gtfs object to just the current day.
daily_gtfs_obj <- function(url, day = Sys.Date()) {
  gtfs_obj <- import_gtfs(url)
  gtfs_drilldown(gtfs_obj, day)
}
