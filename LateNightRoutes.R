#Load libraries
library(tidyverse)
library(lubridate)

#Replace working directory with the location of GTFS files
setwd("")

#Load the stop_times file
stop_times <- read_file("stop_times.txt")
stop_times_table <- read_csv(stop_times,
                             col_types = cols(trip_id = col_guess(),
                                              arrival_time = col_character(),
                                              departure_time = col_character(),
                                              stop_id = col_guess(),
                                              stop_sequence = col_skip(),
                                              stop_headsign = col_skip(),
                                              pickup_type = col_skip(),
                                              drop_off_type = col_skip(),
                                              shape_dist_traveled = col_skip(),
                                              timepoint = col_skip()))
rm(stop_times)

#Quick function to clean GTFS files for appropriate hourly timestamps
func_hr <- function (t) {
  ifelse(t < 24, t, t - 24)
}

stop_times_table$departure_hr <- gsub(":", "",substr(stop_times_table$departure_time,1,2), fixed = TRUE) %>%
  as.integer() %>%
  func_hr()

#periods_hr <- tibble(unique(stop_times_table$departure_hr), 0) %>%
#  `colnames<-`(c("hour", "period")) %>%
#  arrange(hour)

#Categorize stops based on specific time periods (3 AM to 10 PM is outside our study period)
stop_times1 <- stop_times_table %>%
  arrange(departure_hr) %>%
  filter(departure_hr >= 3 & departure_hr <22) %>%
  mutate(period = 1)

stop_times2 <- stop_times_table %>%
  arrange(departure_hr) %>%
  filter(departure_hr >= 22) %>%
  mutate(period = 2)
         
stop_times3 <- stop_times_table %>%
  arrange(departure_time) %>%
  filter(departure_hr <3 | is.na(departure_hr)) %>%
  mutate(period = 3)

stop_periods <- bind_rows(stop_times1, stop_times2, stop_times3) %>%
  subset(select = c(trip_id,
                    stop_id,
                    departure_time,
                    departure_hr,
                    period))

rm(stop_times1, stop_times2, stop_times3, stop_times_table)


#Load the trips file to join trip_ids and shape_ids
trips <- read_file("trips.txt")
trips_table <- read_csv(trips)
rm(trips)

trip_vars <- trips_table %>%
  subset(select = c(trip_id,
                    route_id,
                    service_id,
                    shape_id))

late_routes <- left_join(stop_periods,
                         trip_vars,
                         by = "trip_id")

#An extra step to separate each period into its own column to join to GTFS shapes in ArcPro
late_shapes <- late_routes %>%
  group_by(period) %>%
  summarize(unique(shape_id), .groups = "keep") %>%
  mutate(count = 1) %>%
  pivot_wider(id_cols = "unique(shape_id)",
              names_from = "period",
              names_glue = "period{period}",
              values_from = "count") %>%
  rename("shape_id" = "unique(shape_id)") %>%
  replace_na(list(period1 = 0,
                  period2 = 0,
                  period3 = 0))

#export file  
write_csv(late_shapes, "")

rm(list=setdiff(ls(), "mode2f"))
