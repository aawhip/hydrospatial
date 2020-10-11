#' Prepare daily flows table for hydrospatial analysis
#'
#' Prepares the flows table that will be used in hydrospatial analysis, taking
#' as input the flows table from the 'utils_flowstopredict' function.
#'
#' @param d Data frame in format of 'utils_flowstopredict' function
#' @param sc Scenario name
#' @importFrom lubridate mdy yday
#' @export
#' @return Flows data frame as input for hydrospatial analysis

utils_hsaflws <- function(d, sc){

  d$dt <- ymd(d$dt)
  d$sc <- sc
  d$jd <- yday(d$dt) # jd=Julian day
  d$limb <- as.character(d$limb)

  # Add empty columns
  d$tinun_a <- NA; d$inun_a <- NA; d$pinun_a <- NA
  d$conn_a <- NA; d$dconn_a <- NA
  d$areaminreq <- NA
  d$salwua <- NA; d$salhhs <- NA

  return(d)

}
