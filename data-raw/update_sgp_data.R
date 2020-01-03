update_sgp_data <- function() {
  library(oddsandsods)
  library(magrittr)
  library(dplyr)
  library(RMySQL)
  library(runner)
  con <- dbConnect(drv = MySQL(), 
                   username = "root", 
                   dbname = "speedway", 
                   encoding = "UTF-8")
  dbGetQuery(con, "SET NAMES utf8")
  gpsquads <- customQuery({
    "
    SELECT 
      e.id,
      e.season,
      e.date,
      e.place,
      e.round,
      e.name, 
      s.rider_name rider,
      s.points,
      s.classification
    FROM speedway.event_squads s
    LEFT JOIN speedway.events e on e.id = s.event_id
    WHERE competition = 'Grand-Prix'
    ;"
  })
  gpheats <- customQuery({
    "
    SELECT 
      e.id,
      e.season,
      e.date,
      e.round,
      e.name, 
      h.heat,
      h.field,
      h.rider_name rider,
      h.points,
      h.position
    FROM speedway.event_heats h
    LEFT JOIN speedway.events e on e.id = h.event_id
    WHERE 
      competition = 'Grand-Prix'
    "
  })
  dbDisconnect(con)

  gpsquads$date <- as.POSIXct(strptime(gpsquads$date, "%Y-%m-%d %H:%M:%S"))
  gpheats$date <- as.POSIXct(strptime(gpheats$date, "%Y-%m-%d %H:%M:%S"))

  gpheats <-
    gpheats %>%
    arrange(date, heat) %>%
    mutate(id = runner::sum_run(paste(date, heat) != lag(paste(date, heat), default = ""))) %>%
    filter(!is.na(position)) %>%
    filter(!is.na(points)) %>%
    filter(!position %in% c("F", "N")) %>%
    filter(!is.na(field)) %>%
    filter(!is.na(rider)) %>%
    mutate(
      rank = as.integer(position),
      rank = ifelse(is.na(rank), max(rank, na.rm = T) + 1, rank)
    )

  Encoding(gpheats$name)  <- "UTF-8"
  Encoding(gpheats$rider) <- "UTF-8"
  Encoding(gpsquads$name)  <- "UTF-8"
  Encoding(gpsquads$place) <- "UTF-8"
  Encoding(gpsquads$rider) <- "UTF-8"
  
  
  usethis::use_data(gpsquads, 
                   gpheats, overwrite = TRUE)
}
