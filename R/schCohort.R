#' written by DCConley, DurhamDSB/DurhamDSB and CConley SchoolR
#'
#' @title 'School Year Cohort'
#'
#' @description
#'   Calculates the school year cohort from any given date.
#'
#' @param date vector
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' schCohort(lubridate::now())
#'

schCohort <- function(date) {
  cohortyear <- ifelse(lubridate::month(lubridate::as_date(date)) %in%
                         c(9, 10, 11, 12),
                       paste0(lubridate::year(lubridate::as_date(date)), "-",lubridate::year(lubridate::as_date(date))+1),
                       paste0(lubridate::year(lubridate::as_date(date))-1, "-",lubridate::year(lubridate::as_date(date))))
  return(cohortyear)
}
