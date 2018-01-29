#' Heat results of Speedway Grand-Prix 
#'
#' Actual dataset containing heats results of all Speedway Grand-Prix turnaments
#' gpheats.
#'
#' @format A data frame with >19000 rows and 11 variables:
#' \describe{
#'   \item{id}{event identifier}
#'   \item{season}{year of Grand-Prix, 1995-now}
#'   \item{date}{date of turnament}
#'   \item{round}{round in season}
#'   \item{name}{Turnament name}
#'   \item{place}{stadium of event}
#'   \item{heat}{heat number, 1-23}
#'   \item{field}{number of gate, 1-4}
#'   \item{rider_name}{rider name, string}
#' }
#' @source internal
#' @name gpheats
NULL


#' Turnament results of Speedway Grand-Prix 
#'
#' Actual dataset containing turnament results of all Speedway Grand-Prix events
#' gpsquads
#'
#' @format A data frame with >4000 rows and 9 variables:
#' \describe{
#'   \item{event_id}{event identifier}
#'   \item{season}{year of Grand-Prix, 1995-now}
#'   \item{date}{date of turnament}
#'   \item{round}{round in season}
#'   \item{name}{Turnament name}
#'   \item{place}{stadium of event}
#'   \item{rider_name}{points, 1-6}
#'   \item{classification}{classification after an event}
#' }
#' @source internal
#' @name gpsquads
NULL