#' written by DCConley, DurhamDSB/DurhamDSB and CConley SchoolR
#'#'
#' @title 'expected Year of Birth (YOB)' operator
#'
#' @description
#'   calculated the year a student would be expected to have been born in
#'   based on the current year and the age (or expected age) of the student
#'
#' @param current a vector in the form YYYY
#' @param age a vector
#'
#' @return a numeric value representing the expected Year of Birth (YOB)
#' @export
#'
#' @examples
#' xpYOB(2001, 2018)
#'

xpYOB <- function(current, age) {
  expected <- current - birth - 5
  return(expected)
}


