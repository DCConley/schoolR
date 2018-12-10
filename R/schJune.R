#' written by DCConley, DurhamDSB/DurhamDSB and CConley SchoolR
#'
#' @title 'School June Cohort Year'
#'
#' @description
#'   Calculates the school year as of June from any given date.
#'
#' @param date vector
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' schJune(lubridate::now())
#'


schJune <- function(date) {
  cohortyear <- ifelse(lubridate::month(lubridate::as_date(date)) %in%
                         c(9, 10, 11, 12),
                       as.numeric(lubridate::year(lubridate::as_date(date)))-1,
                       as.numeric(lubridate::year(lubridate::as_date(date))))
  return(cohortyear)
}

