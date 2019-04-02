#' Clean.Dates
#'
#' Converts dates in inconsistent formats into a consistent format.
#' As written it only uses base functions,
#' @param x A vector of dates
#'
#' @return A multicolumn vector of the same length as x with the following values:
#'
#' "YYMMDD", "week", "YYYY", "DD", "MM", "MMDD", "DOY"
#'
#' @export Clean.Dates
#' @examples
#' Clean.Dates(c("2018-05-18","2017-12-12"))
#'
#'
Clean.Dates <- function(x) {
  f.data <- as.data.frame(x)
  # Add a column name
  colnames(f.data) <- c("Date")
  # Rather than use subfunctions we use a series of custom regex to check for basic date syntax.
  # Once the rows containing valid dataes are found they are converted to dates
  # The resutling output is the same length as the input vector so the user has a choice
  # on how to handle the invalid date rows.

  # Test for dates dd-mm-yyyy
  f.data$isadate <- grepl("\\d{2}-\\d{2}-\\d{4}",f.data$Date )
  which.rows <- which(f.data$isadate)
  # We now know which dates are true and which are false.
  f.data[which.rows,"date_format_YYYYMMDD"] <-
    strftime(as.POSIXlt(as.Date(f.data[which.rows,"Date"], origin="1904-01-01", format = "%d-%m-%Y")),
             format="%Y-%m-%d")
  # We need to do this for each date pattern in the data.

  # Test for dates dd/mm/yyyy
  f.data$isadate <- grepl("\\d{2}/\\d{2}/\\d{4}",f.data$Date )
  which.rows <- which(f.data$isadate)
  # We now know which dates are true and which are false.
  f.data[which.rows,"date_format_YYYYMMDD"] <-
    strftime(as.POSIXlt(as.Date(f.data[which.rows,"Date"], origin="1904-01-01", format = "%d/%m/%Y")),
             format="%Y-%m-%d")

  # Test for dates yyyy-mm-dd
  f.data$isadate <- grepl("\\d{4}-\\d{2}-\\d{2}",f.data$Date )
  which.rows <- which(f.data$isadate)
  # We now know which dates are true and which are false.
  f.data[which.rows,"date_format_YYYYMMDD"] <-
    strftime(as.POSIXlt(as.Date(f.data[which.rows,"Date"], origin="1904-01-01", format = "%Y-%m-%d")),
             format="%Y-%m-%d")

  # Test for dates yyyy/mm/dd
  f.data$isadate <- grepl("\\d{4}/\\d{2}/\\d{2}",f.data$Date )
  which.rows <- which(f.data$isadate)
  # We now know which dates are true and which are false.
  f.data[which.rows,"date_format_YYYYMMDD"] <-
    strftime(as.POSIXlt(as.Date(f.data[which.rows,"Date"], origin="1904-01-01", format = "%Y/%m/%d")),
             format="%Y-%m-%d")

  # Test for Excel dates
  f.data$isadate <- grepl("\\d{5}",f.data$Date )
  which.rows <- which(f.data$isadate)
  # We now know which dates are true and which are false.
  f.data[which.rows,"date_format_YYYYMMDD"] <-
    strftime(as.POSIXlt(as.Date(as.numeric(f.data[which.rows,"Date"]), origin = "1899-12-30",format = "%Y-%m-%d" )),
             format="%Y-%m-%d")


  # Now try to transform all the dates and break out the year, month and week numbers
  #f.data$YYMMDD <-format(as.Date(x, origin="1904-01-01", format = "%d/%m/%Y"), "%Y/%m/%d")
  f.data$YYMMDD <- f.data$date_format_YYYYMMDD
  f.data$week <- strftime(as.POSIXlt(as.Date(f.data$YYMMDD, origin="1904-01-01", format = "%Y-%m-%d")), format="%W")
  f.data$YYYY <- strftime(as.POSIXlt(as.Date(f.data$YYMMDD, origin="1904-01-01", format = "%Y-%m-%d")), format="%Y")
  f.data$DD <- strftime(as.POSIXlt(as.Date(f.data$YYMMDD, origin="1904-01-01", format = "%Y-%m-%d")), format="%d")
  f.data$MM <- strftime(as.POSIXlt(as.Date(f.data$YYMMDD, origin="1904-01-01", format = "%Y-%m-%d")), format="%m")
  f.data$MMDD <- strftime(as.POSIXlt(as.Date(f.data$YYMMDD, origin="1904-01-01", format = "%Y-%m-%d")), format="%m%d")
  f.data$DOY  <- strftime(as.POSIXlt(as.Date(f.data$YYMMDD, origin="1904-01-01", format = "%Y-%m-%d")), format="%j")
  # Remove remaining NA dates by testing where we can work out a year

  #f.data <- subset(f.data, !is.na(f.data$YYYY)) %>% as.data.frame

  # And finally drop the columns we do not need by selecting those that are useful
  #f.data <- subset(f.data, select=c("Date","YYMMDD","week","YYYY","DD","MM", "MMDD","DOY"))
  f.data <- f.data[,c("YYMMDD","week","YYYY","DD","MM", "MMDD","DOY")]

}

