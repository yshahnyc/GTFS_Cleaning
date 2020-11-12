library(tidytransit)
library(tidyverse)

region_feed = feedlist %>%
  filter(loc_n %in% c("Durham Regional Municipality",
                    "Hamilton",
                    "Burlington",
                    "Mississauga",
                    "Oakville",
                    "York",
                    "Brampton",
                    "Toronto"))

#load gtfs feed (using TTC as an example here)
City_feed = region_feed %>%
  filter(t %in% "TTC GTFS") %>%
  select(url_d)

City_gtfs = read_gtfs(City_feed[1,])

City_freq = get_route_frequency(City_gtfs,
                                start_hour=22,
                                end_hour=3)
#coerce route_ids
#City_freq$route_id = as.numeric(City_freq$route_id)

City_route_id = City_gtfs$routes$route_id
City_route_name = City_gtfs$routes$route_short_name

City_routes = cbind.data.frame(City_route_id,City_route_name)
colnames(City_routes) = c("route_id", "route_name")

City_freq = left_join(City_freq, City_routes, by = "route_id")

#City_freq %>%
#  write_csv("")
