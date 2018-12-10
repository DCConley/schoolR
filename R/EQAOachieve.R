#' @title 'Achievement Label Recoding'
#'
#' @description
#'   Recodes the achievement levels of school achievement dataframes
#'
#' @param df dataframe of from schoolLoad
#' @param grade vector in the form 3, 6, 9 or 10
#' @param type vector in the form "char" and "num"
#'
#' @return a dataframe with recoded achievement values
#' @export
#'
#' @examples
#' G3school2017 <- EQAOachieve(G3school2017, 3, "char")
#' G6school2017 <- EQAOachieve(G6school2017, 6, "num")
#'

EQAOachieve <- function(x, grade, type){
  ifelse(grade ==3, {
    ifelse(type == "char", {
      x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(ROverallLevel,
                                                       WOverallLevel,
                                                       MOverallLevel),
                            .funs= dplyr::funs(dplyr::recode(.,`1` = "Level 1",
                                                             `2` = "Level 2",
                                                             `3` = "Level 3",
                                                             `4` = "Level 4",
                                                             `0` = "NE1",
                                                             `W` = "Witheld",
                                                             `R` = "Witheld",
                                                             `P` = "Pending",
                                                             `X` = "Exempt",
                                                             `Q` = "Not Required",
                                                             `-1` = "No Data",
                                                             `B` = "No Data")
                            )
      )
    },
    ifelse(type == "num", {
      x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(ROverallLevel,
                                                       WOverallLevel,
                                                       MOverallLevel),
                            .funs= dplyr::funs(dplyr::recode(.,`1` = 1,
                                                             `2` = 2,
                                                             `3` = 3,
                                                             `4` = 4,
                                                             `0` = 0,
                                                             `W` = as.numeric(NA),
                                                             `R` = as.numeric(NA),
                                                             `P` = as.numeric(NA),
                                                             `X` = as.numeric(NA),
                                                             `Q` = as.numeric(NA),
                                                             `-1` = as.numeric(NA),
                                                             `B` = as.numeric(NA))
                            )
      )
    }, "Check the type of recoding selected")
    )
  },
  ifelse(grade == 6, {
    ifelse(type == "char", {
      x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(ROverallLevel,
                                                       WOverallLevel,
                                                       MOverallLevel,
                                                       Prior_G3_ROverallLevel,
                                                       Prior_G3_WOverallLevel,
                                                       Prior_G3_MOverallLevel),
                            .funs= dplyr::funs(dplyr::recode(.,`1` = "Level 1",
                                                             `2` = "Level 2",
                                                             `3` = "Level 3",
                                                             `4` = "Level 4",
                                                             `0` = "NE1",
                                                             `W` = "Witheld",
                                                             `R` = "Witheld",
                                                             `P` = "Pending",
                                                             `X` = "Exempt",
                                                             `Q` = "Not Required",
                                                             `-1` = "No Data",
                                                             `B` = "No Data")
                            )
      )
    },
    ifelse(type == "num", {
      x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(ROverallLevel,
                                                       WOverallLevel,
                                                       MOverallLevel,
                                                       Prior_G3_ROverallLevel,
                                                       Prior_G3_WOverallLevel,
                                                       Prior_G3_MOverallLevel),
                            .funs= dplyr::funs(dplyr::recode(.,`1` = 1,
                                                             `2` = 2,
                                                             `3` = 3,
                                                             `4` = 4,
                                                             `0` = 0,
                                                             `W` = as.numeric(NA),
                                                             `R` = as.numeric(NA),
                                                             `P` = as.numeric(NA),
                                                             `X` = as.numeric(NA),
                                                             `Q` = as.numeric(NA),
                                                             `-1` = as.numeric(NA),
                                                             `B` = as.numeric(NA))
                            )
      )
      x$change_g63_read <- ifelse(!is.na(x$ROverallLevel) &
                                    !is.na(x$Prior_G3_ROverallLevel),
                                  x$ROverallLevel - x$Prior_G3_ROverallLevel, NA)
      x$change_g63_write <- ifelse(!is.na(x$WOverallLevel) &
                                     !is.na(x$Prior_G3_WOverallLevel),
                                   x$WOverallLevel - x$Prior_G3_WOverallLevel, NA)
      x$change_g63_math <- ifelse(!is.na(x$MOverallLevel) &
                                    !is.na(x$Prior_G3_MOverallLevel),
                                  x$MOverallLevel - x$Prior_G3_MOverallLevel, NA)


      x$change_g63_read_label <- ifelse(!is.na(x$change_g63_read),
                                        ifelse(x$change_g63_read > 0 ,
                                               "increase",
                                               ifelse(x$change_g63_read <0,
                                                      "decrease",
                                                      "same")
                                        ), NA
      )
      x$change_g63_write_label <- ifelse(!is.na(x$change_g63_write),
                                         ifelse(x$change_g63_write > 0 ,
                                                "increase",
                                                ifelse(x$change_g63_write <0,
                                                       "decrease",
                                                       "same")
                                         ), NA
      )
      x$change_g63_math_label <- ifelse(!is.na(x$change_g63_math),
                                        ifelse(x$change_g63_math > 0 ,
                                               "increase",
                                               ifelse(x$change_g63_math <0,
                                                      "decrease",
                                                      "same")
                                        ), NA
      )
    }, "Check the type of recoding selected")
    )
  },
  ifelse(grade == 9 , {
    ifelse(type == "char", {
      x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(OverallOutcomeLevel,
                                                       Prior_G3_ROverallLevel,
                                                       Prior_G3_WOverallLevel,
                                                       Prior_G3_MOverallLevel,
                                                       Prior_G6_ROverallLevel,
                                                       Prior_G6_WOverallLevel,
                                                       Prior_G6_MOverallLevel),
                            .funs= dplyr::funs(dplyr::recode(.,`1` = "Level 1",
                                                             `2` = "Level 2",
                                                             `3` = "Level 3",
                                                             `4` = "Level 4",
                                                             `0` = "NE1",
                                                             `W` = "Witheld",
                                                             `R` = "Witheld",
                                                             `P` = "Pending",
                                                             `X` = "Exempt",
                                                             `V` = "Vulgar",
                                                             `Q` = "Not Required",
                                                             `-1` = "No Data",
                                                             `B` = "No Data")
                            )
      )
    },
    ifelse(type == "num", {
      x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(OverallOutcomeLevel,
                                                       Prior_G3_ROverallLevel,
                                                       Prior_G3_WOverallLevel,
                                                       Prior_G3_MOverallLevel,
                                                       Prior_G6_ROverallLevel,
                                                       Prior_G6_WOverallLevel,
                                                       Prior_G6_MOverallLevel),
                            .funs= dplyr::funs(dplyr::recode(.,`1` = 1,
                                                             `2` = 2,
                                                             `3` = 3,
                                                             `4` = 4,
                                                             `0` = 0,
                                                             `W` = as.numeric(NA),
                                                             `R` = as.numeric(NA),
                                                             `P` = as.numeric(NA),
                                                             `X` = as.numeric(NA),
                                                             `V` = as.numeric(NA),
                                                             `Q` = as.numeric(NA),
                                                             `-1` = as.numeric(NA),
                                                             `B` = as.numeric(NA)
                            )
                            )
      )
      x$change_g96_math <- ifelse(!is.na(x$OverallOutcomeLevel) &
                                    !is.na(x$Prior_G6_MOverallLevel),
                                  x$OverallOutcomeLevel - x$Prior_G6_MOverallLevel, NA)
      x$change_g93_math <- ifelse(!is.na(x$OverallOutcomeLevel) &
                                    !is.na(x$Prior_G3_MOverallLevel),
                                  x$OverallOutcomeLevel - x$Prior_G3_MOverallLevel, NA)


      x$change_g96_math_label <- ifelse(!is.na(x$change_g96_math),
                                        ifelse(x$change_g96_math > 0 ,
                                               "increase",
                                               ifelse(x$change_g96_math <0,
                                                      "decrease",
                                                      "same")
                                        ), NA)
      x$change_g93_math_label <- ifelse(!is.na(x$change_g93_math),
                                        ifelse(x$change_g93_math > 0 ,
                                               "increase",
                                               ifelse(x$change_g93_math <0,
                                                      "decrease",
                                                      "same")
                                        ), NA)

    }, "Check the type of recoding selected"))},
    ifelse(grade == 10, {
      ifelse(type == "char", {
        x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(Prior_G6_ROverallLevel,
                                                         Prior_G6_WOverallLevel,
                                                         Prior_G3_ROverallLevel,
                                                         Prior_G3_WOverallLevel),
                              .funs= dplyr::funs(dplyr::recode(.,`1` = "Level 1",
                                                               `2` = "Level 2",
                                                               `3` = "Level 3",
                                                               `4` = "Level 4",
                                                               `0` = "NE1",
                                                               `W` = "Witheld",
                                                               `R` = "Witheld",
                                                               `P` = "Pending",
                                                               `X` = "Exempt",
                                                               `Q` = "Not Required",
                                                               `-1` = "No Data",
                                                               `B` = "No Data")
                              )
        )
        x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(OSSLTOutcome),
                              .funs= dplyr::funs(dplyr::recode(.,`0` = "Pending",
                                                               `1` = "Successful",
                                                               `2` = "Unsuccessful",
                                                               `3` = "Absent",
                                                               `4` = "OSSLC",
                                                               `5` = "Deferred",
                                                               `6` = "Exempt",
                                                               `10` = "Witheld")
                              )
        )
      },
      ifelse(type == "num", {
        x <- dplyr::mutate_at(.tbl=x, .vars= dplyr::vars(OSSLTOutcome),
                              .funs= dplyr::funs(dplyr::recode(.,`0` = as.numeric(NA),
                                                               `1` = 1,
                                                               `2` = 0,
                                                               `3` = as.numeric(NA),
                                                               `4` = 1,
                                                               `5` = as.numeric(NA),
                                                               `6` = as.numeric(NA),
                                                               `10` = as.numeric(NA))
                              )
        )
      }, "Check the type of recoding selected")
      )},"Check the grade selected")
  )
  )
  )
  return(x)
}

