#' written by DCConley, DurhamDSB/DurhamDSB and CConley SchoolR
#'#'
#' @title 'expected age' operator
#'
#' @description
#'   calculated the age a student would be expected to be
#'   based on the current grade a student is in.
#'
#' @param grade a vector
#'
#' @return a numeric value representing the expected age
#' @export
#'
#' @examples
#' xpAge(6, 2018)
#'

xpAge <- function(grade) {
  age <- ifelse(grade == "JK",
                5-1, {
                  ifelse(grade == "SK", 5,
                         as.numeric(grade)+5)
                }
                )
  return(age)
  }
