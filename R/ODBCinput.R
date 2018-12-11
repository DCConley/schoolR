#' written by DCConley, DurhamDSB/DurhamDSB and CConley ddsbR
#'#'
#' @title 'ODBC Input database, username and password'
#'
#' @description
#'  pass the ODBC database, username and password without storing it in code
#'
#' @param x the ODBC database nam in the ODBC manager
#' @param y the username for the ODBC account
#' @param z the password for the ODBC account
#'
#'
#' @return a vector with three values
#' @export
#'
#' @examples
#' DatabaseConnect <- ODBCinput()
#' channel <- odbcConnect(DatabaseConnect[1],
#'                        uid = DatabaseConnect[2],
#'                        pwd=DatabaseConnect[3])


ODBCinput <- function(){

  xvar <- tcltk::tclVar("")
  yvar <- tcltk::tclVar("")
  zvar <- tcltk::tclVar("")

  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt,"Connect to PowerSchool")
  x.entry <- tcltk::tkentry(tt, textvariable=xvar)
  y.entry <- tcltk::tkentry(tt, textvariable=yvar)
  z.entry <- tcltk::tkentry(tt, textvariable=zvar)

  reset <- function()
  {
    tcltk::tclvalue(xvar)<-""
    tcltk::tclvalue(yvar)<-""
    tcltk::tclvalue(zvar)<-""
  }

  reset.but <- tcltk::tkbutton(tt, text="Reset", command=reset)

  submit <- function() {
    x <- tcltk::tclvalue(xvar)
    y <- tcltk::tclvalue(yvar)
    z <- tcltk::tclvalue(zvar)
    e <- parent.env(environment())
    e$x <- x
    e$y <- y
    e$z <- z
    tcltk::tkdestroy(tt)
  }
  submit.but <- tcltk::tkbutton(tt, text="submit", command=submit)

  tcltk::tkgrid(tcltk::tklabel(tt,text="Login Information"),columnspan=2)
  tcltk::tkgrid(tcltk::tklabel(tt,text="ODBC Database name"), x.entry, pady = 10, padx =10)
  tcltk::tkgrid(tcltk::tklabel(tt,text="Username"), y.entry, pady = 10, padx =10)
  tcltk::tkgrid(tcltk::tklabel(tt,text="Password"), z.entry, pady = 10, padx =10)
  tcltk::tkgrid(submit.but, reset.but)

  tcltk::tkwait.window(tt)
  return(c(x,y,z))
}


