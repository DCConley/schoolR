#' @title 'Load EQAO Student ISD csv'
#'
#' @description
#'   Loads Individual Student Data File for an assessment year
#'   into a single dataframe and cleans labels: Exceptionality, Gender
#'   Program, ELL, FI
#'
#' @param grade vector in the form 3, 6, 9 or 10
#' @param year vector in the form ####
#' @param bident vector in the form #####
#' @param datadir vector in the form "C:/Data/"
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' EQAOstudent(3, 2018, 66060, "C:/Data/")
#'


EQAOstudent <- function(grade, year, bident, datadir){
  datadir.txt <- ifelse(datadir == "v", "V:/Programs/Accountability & Assessment/AA_MasterData/",
                        ifelse(datadir == "c", "C:/Data-Local/", datadir))
  ifelse(grade == 10, {
    x <- readr::read_csv(paste0(datadir.txt,"OSSLT_",year,"_ISD_SQ_B0E",bident,".csv"))

    x$IEPcode <- ifelse(x$IPRCExBehaviour == 1, "Behaviour",
                        ifelse(x$IPRCExAutism == 1, "Autism",
                               ifelse(x$IPRCExDeaf == 1, "Deaf",
                                      ifelse(x$IPRCExLanguage == 1, "Language",
                                             ifelse(x$IPRCExSpeech == 1, "Speech",
                                                    ifelse(x$IPRCExLearning == 1, "Learning",
                                                           ifelse(x$IPRCExGiftedness == 1, "Gifted",
                                                                  ifelse(x$IPRCExMildIntellectual == 1, "Mild Intellectual",
                                                                         ifelse(x$IPRCExDevelopmental == 1, "Developmental",
                                                                                ifelse(x$IPRCExPhysical == 1, "Physical",
                                                                                       ifelse(x$IPRCExBlind == 1, "Blind",
                                                                                              ifelse(x$IPRCExMultiple == 1, "Multiple", "No IEP")
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
                        )
    )

    x$Gender <- ifelse(x$Gender == 1, "Male",
                       ifelse(x$Gender == 2, "Female", NA))

    x$EligibilityStatus <- ifelse(x$EligibilityStatus == 2, "PE",
                                  ifelse(x$EligibilityStatus == 1, "FTE", NA))

    x$LevelOfStudyLanguage <- ifelse(x$LevelOfStudyLanguage == 5, "Other",
                                     ifelse(x$LevelOfStudyLanguage == 4, "ESL/ELD",
                                            ifelse(x$LevelOfStudyLanguage == 3, "Loc. Dev.",
                                                   ifelse(x$LevelOfStudyLanguage == 2, "Applied",
                                                          ifelse(x$LevelOfStudyLanguage == 1, "Academic", NA)))))
    x$ESLELD_ALFPDF <- ifelse(x$ESLELD_ALFPDF == 1, "ELL",
                              ifelse(x$ESLELD_ALFPDF == 0, "non-ELL", NA))

    x$AccScribing <- ifelse(x$AccScribing == 1, "Scribe",
                            ifelse(x$AccScribing == 0, "no Scribe", NA))

    x$AccAssistiveTech <- ifelse(x$AccAssistiveTech == 1, "Assist. Tech.",
                                 ifelse(x$AccAssistiveTech == 0, "no Assist. Tech", NA))
    return(x)
  },
  ifelse(grade == 9, {
    x <- readr::read_csv(paste0(datadir.txt,"G9_",year,"_B0E", bident,"_ISD_SQ.csv"))

    x$IEPcode <- ifelse(x$SIF_IPRCBehaviour == 1, "Behaviour",
                        ifelse(x$SIF_IPRCAutism == 1, "Autism",
                               ifelse(x$SIF_IPRCDeaf == 1, "Deaf",
                                      ifelse(x$SIF_IPRCLanguage == 1, "Language",
                                             ifelse(x$SIF_IPRCSpeech == 1, "Speech",
                                                    ifelse(x$SIF_IPRCLearning == 1, "Learning",
                                                           ifelse(x$SIF_IPRCGifted == 1, "Gifted",
                                                                  ifelse(x$SIF_IPRCIntellectual == 1, "Mild Intellectual",
                                                                         ifelse(x$SIF_IPRCDevelopmental == 1, "Developmental",
                                                                                ifelse(x$SIF_IPRCPhysical == 1, "Physical",
                                                                                       ifelse(x$SIF_IPRCBlind == 1, "Blind",
                                                                                              ifelse(x$SIF_IPRCMultiple == 1, "Multiple", "No IEP")
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
                        )
    )
    x$Gender <- ifelse(x$Gender == 1, "Male",
                       ifelse(x$Gender == 2, "Female", NA))

    x$Program <- ifelse(x$Program == 2, "Academic",
                        ifelse(x$Program == 1, "Applied", NA))

    x$MathClassWhen <- ifelse(x$MathClassWhen == 1, "Semester 1",
                              ifelse(x$MathClassWhen == 2, "Semester 2",
                                     ifelse(x$MathClassWhen == 3, "Full Year",NA)))
    x$ESLELD_ALFPDF <- ifelse(x$ESLELD_ALFPDF == 1, "ELL",
                              ifelse(x$ESLELD_ALFPDF == 0, "non-ELL", NA))
    return(x)
  },
  ifelse(grade %in% c(3,6), {
    x <- readr::read_csv(paste0(datadir.txt,year-1, year-2000,"_B",bident,"_1G",grade,".csv"))

    x$IEPcode <- ifelse(x$SIF_IPRC_Behaviour == 1, "Behaviour",
                        ifelse(x$SIF_IPRC_Autism == 1, "Autism",
                               ifelse(x$SIF_IPRC_Deaf == 1, "Deaf",
                                      ifelse(x$SIF_IPRC_Language == 1, "Language",
                                             ifelse(x$SIF_IPRC_Speech == 1, "Speech",
                                                    ifelse(x$SIF_IPRC_Learning == 1, "Learning",
                                                           ifelse(x$SIF_IPRC_Giftedness == 1, "Gifted",
                                                                  ifelse(x$SIF_IPRC_MildIntellectual == 1, "Mild Intellectual",
                                                                         ifelse(x$SIF_IPRC_Developmental == 1, "Developmental",
                                                                                ifelse(x$SIF_IPRC_Physical == 1, "Physical",
                                                                                       ifelse(x$SIF_IPRC_Blind == 1, "Blind",
                                                                                              ifelse(x$SIF_IPRC_Multiple == 1, "Multiple", "No IEP")
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
                        )
    )
    x$Gender <- ifelse(x$Gender == 1, "Male",
                       ifelse(x$Gender == 2, "Female", NA))

    x$Background_FrenchImmersion <- ifelse(x$Background_FrenchImmersion == 1, "FI",
                                           ifelse(x$Background_FrenchImmersion == 0, "not FI", NA))

    x$Background_ESLELD_ALFPDF <- ifelse(x$Background_ESLELD_ALFPDF == 1, "ELL",
                                         ifelse(x$Background_ESLELD_ALFPDF == 0, "not ELL", NA))

    return(x)
  },"Unable to locate files, check the grade, bident, or file name.")))
}

