#' written by DCConley, DurhamDSB/DurhamDSB and CConley SchoolR
#'
#' @title 'School September Cohort Year'
#'
#' @description
#'   Calculates the school year as of September from any given date.
#'
#' @param date vector
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' schSept(lubridate::now())
#'

schSept <- function(date) {
  cohortyear <- ifelse(lubridate::month(lubridate::as_date(date)) %in%
                         c(9, 10, 11, 12),
                       as.numeric(lubridate::year(lubridate::as_date(date))),
                       as.numeric(lubridate::year(lubridate::as_date(date)))-1)
  return(cohortyear)
}
