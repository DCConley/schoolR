#' @title 'Load EQAO Provincial csv'
#'
#' @description
#'   Loads all EQAO Provincial files for an assessment year
#'   into a single dataframe
#'
#' @param grade vector in the form 3, 6, 9 or 10
#' @param year vector in the form ####
#' @param datadir vector in the form "C:/Data/"
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' EQAOprov(3, 2017, "C:/Data/")
#'

EQAOprov <- function(grade, year, datadir) {
  datadir.txt <- ifelse(datadir == "v", "V:/Programs/Accountability & Assessment/AA_MasterData/",
                        ifelse(datadir == "c", "C:/Data-Local/", datadir))

  ifelse(grade == 10, {
    OSSLT.1 <- readr::read_csv(paste0(datadir.txt,
                                      "G10_",
                                      year,
                                      "_FTE_P01_1.csv"))
    OSSLT.1$SchoolMident[is.na(OSSLT.1$SchoolMident)] <- 1
    OSSLT.1$SchoolName[is.na(OSSLT.1$SchoolName)] <- "Prov"

    OSSLT.2 <- readr::read_csv(paste0(datadir.txt,
                                      "G10_",
                                      year,
                                      "_FTE_P01_2.csv"))
    OSSLT.2$SchoolMident[is.na(OSSLT.2$SchoolMident)] <- 1
    OSSLT.2$SchoolName[is.na(OSSLT.2$SchoolName)] <- "Prov"
    OSSLT.2 <- dplyr::select(OSSLT.2,
                             -OrgType,
                             -Lang,
                             -ApplySuppression,
                             -BoardMident,
                             -BoardName,
                             -SchoolName,
                             -Suppressed,
                             -FundingType,
                             -Eligibility)

    OSSLT.3 <- readr::read_csv(paste0(datadir.txt,
                                      "G10_",
                                      year,
                                      "_FTE_P01_3.csv"))
    OSSLT.3$SchoolMident[is.na(OSSLT.3$SchoolMident)] <- 1
    OSSLT.3$SchoolName[is.na(OSSLT.3$SchoolName)] <- "Prov"
    OSSLT.3 <- dplyr::select(OSSLT.3,
                             -OrgType,
                             -Lang,
                             -ApplySuppression,
                             -BoardMident,
                             -BoardName,
                             -SchoolName,
                             -Suppressed,
                             -FundingType,
                             -Eligibility)

    x <- merge(OSSLT.1, OSSLT.2,
               by = "SchoolMident")
    x <- merge(x, OSSLT.3,
               by = "SchoolMident")

    x$year <- year

    return(x)
  },
  ifelse(grade %in% 9, {
    G9.1 <- readr::read_csv(paste0(datadir.txt,
                                   "G9_",
                                   year,
                                   "_P01_1.csv"))
    G9.1$SchoolMident[is.na(G9.1$SchoolMident)] <- 1
    G9.1$SchoolName[is.na(G9.1$SchoolName)] <- "Prov"

    G9.2 <- readr::read_csv(paste0(datadir.txt,
                                   "G9_",
                                   year,
                                   "_P01_2.csv"))
    G9.2$SchoolMident[is.na(G9.2$SchoolMident)] <- 1
    G9.2$SchoolName[is.na(G9.2$SchoolName)] <- "Prov"
    G9.2 <- dplyr::select(G9.2,
                          -OrgType,
                          -Language,
                          -ApplySuppression,
                          -BoardMident,
                          -BoardName,
                          -SchoolName,
                          -Suppressed,
                          -FundingType)

    G9.3 <- readr::read_csv(paste0(datadir.txt,
                                   "G9_",
                                   year,
                                   "_P01_3.csv"))
    G9.3$SchoolMident[is.na(G9.3$SchoolMident)] <- 1
    G9.3$SchoolName[is.na(G9.3$SchoolName)] <- "Prov"
    G9.3 <- dplyr::select(G9.3,
                          -OrgType,
                          -Language,
                          -ApplySuppression,
                          -BoardMident,
                          -BoardName,
                          -SchoolName,
                          -Suppressed,
                          -FundingType)

    x <- merge(G9.1, G9.2,
               by = "SchoolMident")
    x <- merge(x, G9.3,
               by = "SchoolMident")

    x$year <- year

    return(x)

  },
  ifelse(grade %in% c(3,6), {
    x.1 <- readr::read_csv(paste0(datadir.txt,
                                  "G",
                                  grade,
                                  "_",
                                  year,
                                  "_P01_1.csv"))
    x.1$SchoolMident[is.na(x.1$SchoolMident)] <- 1
    x.1$SchoolName[is.na(x.1$SchoolName)] <- "Prov"

    x.2 <- readr::read_csv(paste0(datadir.txt,
                                  "G",
                                  grade,
                                  "_",
                                  year,
                                  "_P01_2.csv"))
    x.2$SchoolMident[is.na(x.2$SchoolMident)] <- 1
    x.2$SchoolName[is.na(x.2$SchoolName)] <- "Prov"
    x.2 <- dplyr::select(x.2,
                         -OrgType,
                         -Language,
                         -ApplySuppression,
                         -BoardMident,
                         -BoardName,
                         -SchoolName,
                         -Suppressed,
                         -FundingType)

    x.3 <- readr::read_csv(paste0(datadir.txt,
                                  "G",
                                  grade,
                                  "_",
                                  year,
                                  "_P01_3.csv"))
    x.3$SchoolMident[is.na(x.3$SchoolMident)] <- 1
    x.3$SchoolName[is.na(x.3$SchoolName)] <- "Prov"
    x.3 <- dplyr::select(x.3,
                         -OrgType,
                         -Language,
                         -ApplySuppression,
                         -BoardMident,
                         -BoardName,
                         -SchoolName,
                         -Suppressed,
                         -FundingType)

    x.4 <- readr::read_csv(paste0(datadir.txt,
                                  "G",
                                  grade,
                                  "_",
                                  year,
                                  "_P01_4.csv"))
    x.4$SchoolMident[is.na(x.4$SchoolMident)] <- 1
    x.4$SchoolName[is.na(x.4$SchoolName)] <- "Prov"
    x.4 <- dplyr::select(x.4,
                         -OrgType,
                         -Language,
                         -ApplySuppression,
                         -BoardMident,
                         -BoardName,
                         -SchoolName,
                         -Suppressed,
                         -FundingType)

    x <- merge(x.1, x.2,
               by = "SchoolMident")
    x <- merge(x, x.3,
               by = "SchoolMident")
    x <- merge(x, x.4,
               by = "SchoolMident")

    x$year <- year

    return(x)
  }, "Unable to locate files, check the grade, or file name.")))

}

