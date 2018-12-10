#' written by DCConley, DurhamDSB/DurhamDSB and CConley SchoolR
#'
#' @title 'Power School Term ID'
#'
#' @description
#'   Calculates the Power School Term ID for any given year.
#'
#' @param vector in the form YYYY
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' PSTerm(2018)
#' 2800

PSTerm <- function(year) {
  year.num <- as.numeric(paste0(as.numeric(stringr::str_sub(as.character(year),
                                                            3,4))+10 ,"00"))
  return(year.num)
}
