gtfs_drilldown <- function(gtfs_obj, today = Sys.Date()) {
  ### FILTER GTFS DATA FOR WHAT IS RELEVENT TODAY.

  # Get the current service id (for day of week and calendar date)
  todays_service_id <-
    gtfs_obj$calendar_df %>% gather(day, yes, -service_id, -start_date, -end_date) %>%
    filter(yes == 1,
           today >=  ymd(start_date),
           today <= ymd(end_date),
           tolower(strftime(today, "%A")) == day) %>% .$service_id

  # Filter trip_ids for trips running today.
  todays_trips <- gtfs_obj$trips_df %>% filter(service_id == todays_service_id)

  # Filter the trips in stop times for ones running today.
  today_stop_times <- gtfs_obj$stop_times_df %>% filter(trip_id %in% todays_trips$trip_id)

  all_stop_sequences <- today_stop_times %>%
    inner_join(todays_trips, by = "trip_id") %>%
    group_by(route_id, direction_id, stop_sequence) %>%
    filter(row_number() == 2) %>%
    select(route_id, direction_id, stop_id, stop_sequence) %>%
    left_join(gtfs_obj$stops_df, by = "stop_id")


  # Set up static routes to be indexed by bus route_id, direction_id, and order in time.
  todays_trip_departures <-
    gtfs_obj$stop_times_df %>% filter(stop_sequence == 1)  %>%    # Look at when each trip starts.
    inner_join(todays_trips, by = "trip_id") %>%            # Join to get the trip_id info.
    mutate(departure_time = ymd_hms(paste(today,
                                          departure_time, tz = "Anchorage/America"))) %>%  # Convert string to datetime.
    group_by(route_id, direction_id) %>%                    # Group by distinquishable route
    mutate(trip_order_in_route = order(departure_time)) %>% # Order distiquishable routes.
    select(trip_order_in_route, trip_id, route_id, direction_id, departure_time) # Filter rows.

  list(todays_service_id = todays_service_id,
       todays_trips = todays_trips,
       today_stop_times = today_stop_times,
       all_stop_sequences = all_stop_sequences,
       todays_trip_departures = todays_trip_departures,
       stops = gtfs_obj$stops_df)
}
