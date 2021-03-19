#' Heat results of Speedway Grand-Prix
#'
#' Dataset containing heats results of all Speedway Grand-Prix turnaments
#' \code{gpheats}.
#'
#' @format A data frame with >21000 rows and 11 variables:
#' \describe{
#'   \item{id}{event identifier}
#'   \item{season}{year of Grand-Prix, 1995-2019}
#'   \item{date}{date of turnament}
#'   \item{round}{round in season}
#'   \item{name}{Turnament name}
#'   \item{heat}{heat number, 1-23}
#'   \item{field}{number of gate, 1-4}
#'   \item{rider}{rider name, string}
#'   \item{points}{paints gained, integer}
#'   \item{position}{position at finish line, string}
#'   \item{rank}{rank at finish line, integer}
#' }
#' @source internal
#' @name gpheats
NULL


#' Turnament results of Speedway Grand-Prix
#'
#' Dataset containing turnament results of all Speedway Grand-Prix events
#' \code{gpsquads}
#'
#' @format A data frame with >4000 rows and 9 variables:
#' \describe{
#'   \item{id}{event identifier}
#'   \item{season}{year of Grand-Prix, 1995-2019}
#'   \item{date}{date of turnament}
#'   \item{place}{stadium of event}
#'   \item{round}{round in season}
#'   \item{name}{Turnament name}
#'   \item{rider}{rider names, 1-6}
#'   \item{points}{points gained, integer}
#'   \item{classification}{classification after an event}
#' }
#' @source internal
#' @name gpsquads
NULL

#' Heat results from Polish Speedway League
#'
#' Dataset containing heats results from Polish Speedway League
#' \code{plheats}.
#'
#' @format A data frame with >170000 rows and 11 variables:
#' \describe{
#'   \item{id}{event identifier}
#'   \item{season}{year of Grand-Prix, 2004-2019}
#'   \item{season}{year of Grand-Prix, 2004-2019}
#'   \item{date}{date of turnament}
#'   \item{round}{round in season}
#'   \item{name}{Turnament name}
#'   \item{heat}{heat number, 1-15}
#'   \item{field}{number of gate, 1-4}
#'   \item{rider}{rider name, string}
#'   \item{points}{paints gained, integer}
#'   \item{position}{position at finish line, string}
#'   \item{rank}{rank at finish line, integer}
#' }
#' @source internal
#' @name plheats
NULL

#' Results from Polish Speedway League
#'
#' Dataset containing results from Polish Speedway League
#'  events \code{plsquads}
#'
#' @format A data frame with >4000 rows and 9 variables:
#' \describe{
#'   \item{id}{event identifier}
#'   \item{season}{year of Grand-Prix, 2004-2019}
#'   \item{date}{date of turnament}
#'   \item{place}{stadium of event}
#'   \item{round}{round in season}
#'   \item{name}{Turnament name}
#'   \item{rider}{rider names, 1-6}
#'   \item{team_name}{rider names, 1-6}
#'   \item{points}{points gained, integer}
#'   \item{classification}{classification after an event}
#' }
#' @source internal
#' @name plsquads
NULL