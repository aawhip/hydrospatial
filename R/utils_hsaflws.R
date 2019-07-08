#' Function to prepare daily flows table for hydrospatial analysis
#'
#' This function prepares the flows table that will be used in
#' hydrospatial analysis, taking as input the flows table from the
#' 'utils_flowstopredict' function.
#'
#' @param d Data frame in format of 'utils_flowstopredict' function
#' @param sc Scenario name
#' @importFrom lubridate mdy yday
#' @export
#' @return Flows data frame as input for hydrospatial analysis

utils_hsaflws <- function(d, sc){

  d$dt <- mdy(d$dt)
  d$sc <- sc
  d$jd <- yday(d$dt) # jd=Julian day
  d$limb <- as.character(d$limb)

  # Add empty columns
  d$tinun.a <- NA; d$inun.a <- NA; d$pinun.a <- NA
  d$conn.a <- NA; d$dconn.a <- NA
  d$areaminreq <- NA
  d$salwua <- NA; d$salhhs <- NA

  return(d)

}
