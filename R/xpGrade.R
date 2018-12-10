#' written by DCConley, DurhamDSB/DurhamDSB and CConley SchoolR
#'#'
#' @title 'expected grade' operator
#'
#' @description
#'   calculated the grade a student would be expected to be in
#'   based on the current year and the year they were born.
#'
#' @param birth a vector in the form YYYY
#' @param current a vector in the form YYYY
#'
#' @return a numeric value representing the expected grade
#' @export
#'
#' @examples
#' xpGrade(2001, 2018)
#'

xpGrade <- function(birth, current) {
  expected <- current - birth - 5

  expectedreturn <- ifelse(expected == 0, "SK", {
                            ifelse(expected == -1, "JK", {
                              ifelse(expected >0, expected, "Not School Age")
                                                         })
                                                })
  return(expectedreturn)
}
