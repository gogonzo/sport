#' Heat results of Speedway Grand-Prix
#'
#' Actual dataset containing heats results of all Speedway Grand-Prix turnaments
#' `gpheats`.
#'
#' @format A data frame with >19000 rows and 11 variables:
#'   - `id` - event identifier
#'   - `season` - year of Grand-Prix, 1995-now
#'   - `date` - date of turnament
#'   - `round` - round in season
#'   - `name` - Turnament name
#'   - `heat` - heat number, 1-23
#'   - `field` - number of gate, 1-4
#'   - `rider` - rider name, string
#'   - `points` - paints gained, integer
#'   - `position` - position at finish line, string
#'   - `rank` - rank at finish line, integer
#' @source internal
#' @name gpheats
NULL

#' Turnament results of Speedway Grand-Prix
#'
#' Actual dataset containing turnament results of all Speedway Grand-Prix events
#' `gpsquads`
#'
#' @format A data frame with >4000 rows and 9 variables:
#'   - `id` - event identifier
#'   - `season` - year of Grand-Prix, 1995-now
#'   - `date` - date of turnament
#'   - `place` - stadium of event
#'   - `round` - round in season
#'   - `name` - Turnament name
#'   - `rider` - rider names, 1-6
#'   - `points` - points gained, integer
#'   - `classification` - classification after an event
#' @source internal
#' @name gpsquads
NULL
