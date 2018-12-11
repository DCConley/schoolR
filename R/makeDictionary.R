#' written by DCConley, DurhamDSB/DurhamDSB and CConley SchoolR
#'#'
#' @title 'SPSS Data Dictionary'
#'
#' @description
#'   creates a dataframe with all the variable names, values and labels
#'   contained in an SPSS file.  Currently works for variables with scales
#'   up to 9 items.
#'
#' @param savfile a vector
#'
#' @return a dataframe with variable names, values and labels
#' @export
#'
#' @examples
#' makeDictionary("C:/Temp/test.sav")
#'



makeDictionary <- function(savfile){
  df <- haven::read_sav(savfile)

  Questions <- as.data.frame(label(df))
  Questions <- tibble::rownames_to_column(Questions, "name")

  #Get the labels and scales that are used from the list
  Labels <- tibble::enframe(sjlabelled::get_labels(df, values=TRUE))
  Scales <- tibble::enframe(sjlabelled::get_values(df))

  #extract each of the label values
  LabelClean <- Labels
  LabelClean <- dplyr::mutate(LabelClean, Label = gsub('"', "", value ),
                              Label = gsub('c\\(', "", Label),
                              Label = gsub('\\)', "", Label),
                              Label = gsub('"', "", Label ),
                              Label = gsub('c\\(', "", Label),
                              Label = gsub('\\)', "", Label))
  LabelClean <- dplyr::select(LabelClean, -value)
  LabelClean <- LabelClean[-1,]  #remove collector
  LabelClean <- LabelClean[-1,]  #remove school
  LabelClean <- tidyr::separate(LabelClean, Label, into = c("a", "b", "c", "d", "e", "f", "g", "h", "i"), sep = ",")
  LabelClean <- tidyr::gather(LabelClean, 2:10, key = "Label", value = "Scale")
  LabelClean <- dplyr::mutate(LabelClean, uniqueid = paste0(name, "-", Label))

  #extract each of the scale values
  ScalesClean <- Scales
  ScalesClean <- dplyr::mutate(ScalesClean,Label = gsub('"', "", value ),
                               Label = gsub('c\\(', "", Label),
                               Label = gsub('\\)', "", Label),
                               Label = gsub('"', "", Label ),
                               Label = gsub('c\\(', "", Label),
                               Label = gsub('\\)', "", Label))
  ScalesClean <- dplyr::select(ScalesClean,-value)
  ScalesClean <- ScalesClean[-1,]  #remove collector
  ScalesClean <- ScalesClean[-1,]  #remove school
  ScalesClean <- tidyr::separate(ScalesClean, Label, into = c("a", "b", "c", "d", "e", "f", "g", "h", "i"), sep = ",")
  ScalesClean <- tidyr::gather(ScalesClean, 2:10, key = "Label", value = "Scale")
  ScalesClean <- dplyr::mutate(ScalesClean, uniqueid = paste0(name, "-", Label))

  #combined the cleaned labels and scales into a single dataframe
  LabelScales <- merge(LabelClean, ScalesClean, by="uniqueid")
  LabelScales <- dplyr::select(LabelScales,uniqueid,
                               name=name.x,
                               Label = Scale.x,
                               Scale = Scale.y)

  #join the scales to the question stem
  Questions2 <- dplyr::left_join(Questions, LabelScales, by="name")
  Questions2 <- dplyr::filter(Questions2, !is.na(Scale))
  Questions2$Label <- trimws(Questions2$Label, "l")
  Questions2$Scale <- trimws(Questions2$Scale, "l")

  Questions2$QScaleID <- paste0(Questions2$name, "-", Questions2$Scale)
  #Questions2 <- select(Questions2, -)

  return(Questions2)
}

