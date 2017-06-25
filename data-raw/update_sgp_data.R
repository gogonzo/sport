library(oddsandsods);library(magrittr);library(dplyr);library(RMySQL)
con <- dbConnect(drv=MySQL(), username="root", dbname="speedway")
gpsquads <- customQuery({"
  SELECT 
    e.id,
    e.season,
    e.round,
    e.name, 
    s.rider_name,
    s.points,
    s.classification
  FROM speedway.event_squads s
  LEFT JOIN speedway.events e on e.id = s.event_id
  WHERE competition = 'Grand-Prix'
  ;"})
gpheats  <- customQuery({"
  SELECT 
    e.id,
    e.season,
    e.round,
    e.name, 
    h.heat,
    h.field,
    h.rider_name,
    h.points,
    h.position
  FROM speedway.event_heats h
  LEFT JOIN speedway.events e on e.id = h.event_id
  WHERE 
    competition = 'Grand-Prix'
  "})
dbDisconnect(con)

devtools::use_data(gpsquads,gpheats, overwrite = T)
