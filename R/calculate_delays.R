#' Use the gps and gtfs to find which possible trip ids, then assign them based on some logic. Ends up with a object of trip_ids with gps "id".
#'
#' @param my_gps_data the tidy gps table from an agency, includes latitude, longitude, route, direction, and datetime.
#' @param gtfs_today the result of daily_gtfs_obj()
#' @param lat_factor the ratio difference between the length of a degree of longitude / latitude.  To be replaced with the haversine formula based on the gtfs stops table next.
#' @return The sum of \code{x} and \code{y}.
calculate_delays <- function(tidy_gps_obj, gtfs_today, lat_factor) {
  all_delays <- data.frame()
  for(i in unique(tidy_gps_obj$route))  {
    for(j in unique(tidy_gps_obj$direction)) {

      # interate over each route-direction combo.
      gps_data <- tidy_gps_obj %>% filter(route == i, direction == j)
      if(nrow(gps_data) == 0) {next}
      # number of trips currently on the route and direction
      n_trips <- nrow(gps_data)
      # capture as a time object for the day
      current_time <-
        hours(hour(gps_data$datetime[1])) + minutes(minute(gps_data$datetime[1])) + seconds(second(gps_data$datetime[1]))


      if(gtfs_today$todays_trips %>% filter(route_id == i, direction_id == j) %>% nrow() == 0) {next}
      # get the trip ids for this route and direction (filter trips.txt)
      trip_ids_now <-   # (filter stop_times.txt)
        gtfs_today$today_stop_times %>%
        filter(trip_id %in% (gtfs_today$todays_trips %>% filter(route_id == i, direction_id == j) %>% .$trip_id)) %>%
        group_by(trip_id) %>%
        filter(min(stop_sequence) == stop_sequence) %>%
        ungroup() %>%
        filter(hms(departure_time) < current_time) %>%
        arrange(desc(departure_time)) %>%
        filter(row_number() %in% 1:n_trips) %>%
        .$trip_id
      # get route with all stops
      set_of_stops_in_active_trip_ids <-
        gtfs_today$today_stop_times %>%
        filter(trip_id %in% trip_ids_now) %>%
        inner_join(gtfs_today$stops, by = "stop_id") %>%
        select(stop_lat, stop_lon, stop_id, stop_sequence, trip_id, departure_time)

      # set up the possible combinations of distances for trip id and bus gps points
      gtfs_gps_join_prep  <- set_of_stops_in_active_trip_ids[rep(1:nrow(set_of_stops_in_active_trip_ids), n_trips),] # duplicate the dataframe
      gps_data_new    <- gps_data[rep(1:nrow(gps_data), n_trips),]
      gtfs_gps_join_prep$primary_id   <- paste(gtfs_gps_join_prep$trip_id, # add the key for the combination
                                               rep(1:n_trips, each=nrow(set_of_stops_in_active_trip_ids)), sep="-")
      gps_data_new$primary_id     <- paste(trip_ids_now,
                                           rep(1:n_trips, each=nrow(gps_data)), sep = "-")
      gps_data_new$trip_id    <- paste(trip_ids_now)

      # combine gps and gtfs tables
      calc_trip_id_table <- inner_join(gtfs_gps_join_prep, gps_data_new %>% select(-trip_id), by = "primary_id") %>%
        mutate(dist = sqrt((((stop_lat - lat) * lat_factor)^2) +
                             ((stop_lon - lon)^2))) %>%
        group_by(trip_id)
      # get stops_for most recent and next stop and bind them into one object
      A_Stop <- rbind(
        calc_trip_id_table %>%
          filter(min(dist) == dist) %>% filter(row_number() == 1) %>%  mutate(point_type = "most_recent"),
        calc_trip_id_table %>% filter(!min(dist) == dist) %>%
          filter(min(dist) == dist) %>% filter(row_number() == 1) %>%  mutate(point_type = "next_stop"  )) %>%
        group_by(trip_id) %>% filter(min(stop_sequence) == stop_sequence) %>%
        select(gps_lat = lat, gps_lon = lon, A_lat = stop_lat, A_lon = stop_lon, departure_time, trip_id, datetime, stop_sequence)
      #
      surrounding_stops <-
        inner_join(A_Stop, gtfs_today$today_stop_times %>% select(-departure_time),         by = "trip_id") %>%
        inner_join((gtfs_today$all_stop_sequences %>% ungroup() %>% select(stop_lat, stop_lon, stop_id)), by = "stop_id") %>% select(-stop_id) %>%
        filter(stop_sequence.y > stop_sequence.x) %>%
        arrange(stop_sequence.y) %>%
        filter(row_number() == 1) %>%
        select(B_lat = stop_lat, B_lon = stop_lon, A_lat, A_lon, gps_lat, gps_lon, departure_time, arrival_time, trip_id, stop_sequence = stop_sequence.x) %>%
        mutate(A_dist = sqrt((((A_lat - gps_lat) * lat_factor)^2) + ((A_lon - gps_lon)^2)),
               B_dist = sqrt((((B_lat - gps_lat) * lat_factor)^2) + ((B_lon - gps_lon)^2)),
               ratio_complete = A_dist / (A_dist + B_dist),
               delay = round(seconds(current_time - hms(departure_time))  + (seconds((hms(arrival_time) - hms(departure_time))) * ratio_complete)))

      x <- surrounding_stops %>% #select(trip_id, stop_sequence, delay) %>%
        ungroup() %>%
        mutate(route = i, direction = j)

      all_delays <- rbind(all_delays, x )
    }
  }
  return(all_delays)
}
