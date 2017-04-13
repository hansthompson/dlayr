# Filter the gtfs object to just the current day.
daily_gtfs_obj <- function(url, day = Sys.Date()) {
  gtfs_obj <- import_gtfs(url)
  gtfs_drilldown(gtfs_obj, day)
}
