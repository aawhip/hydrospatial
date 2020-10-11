#' Format daily results into continuous time series
#'
#' Prepares the daily hsa results table from hydrospatial analysis and
#' transforms it into a continuous time series for use in visualizing.
#'
#' @param d Data frame in format used for 'hsa' functions
#' @param daily Data frame of continous daily flow record
#' @param sc Scenario name to add as a column
#' @importFrom lubridate  ymd yday
#' @importFrom dplyr left_join
#' @export
#' @return Flows data frame as input for hydrospatial analysis

utils_rsltsformat_daily <- function(d, daily, sc){

  daily$dt <- ymd(as.character(daily$dt))

  d$dt <- ymd(as.character(d$dt))
  d$sc <- as.character(d$sc)

  # Add 0 to missing days to create continuous flows data frame
    time_min <- ymd(paste0((d$wyr[order(d$wyr)][1]-1), "-10-01")) # min date in the series
    time_max <- ymd(paste0((d$wyr[order(d$wyr)][length(d$wyr)]), "-09-30")) # max date in the series
    all_dates <- data.frame(list(dt=seq(time_min, time_max, by="day"))) # Generate the time sequence and make as a data frame
    flws_mrg <- left_join(daily, d)
    flws_mrg <- flws_mrg[flws_mrg$dt>=time_min & flws_mrg$dt<=time_max, ]
    flws_mrg$sc <- sc
    flws_mrg[c("tinun_a","inun_a", "pinun_a",
               "conn_a","dconn_a",
               "salwua")][is.na(flws_mrg[c("tinun_a","inun_a", "pinun_a",
                                           "conn_a","dconn_a",
                                           "salwua")])] <- 0

  # Add cumulative sums to the data frames for period of record
    flws_mrg$tinun_a_cm <- cumsum(flws_mrg$tinun_a)
    flws_mrg$inun_a_cm <- cumsum(flws_mrg$inun_a)
    flws_mrg$conn_a_cm <- cumsum(flws_mrg$conn_a)
    flws_mrg$dconn_a_cm <- cumsum(flws_mrg$dconn_a)
    flws_mrg$salwua_cm <- cumsum(flws_mrg$salwua)

  # Add cumulative for each year
  flws_mrg <- data.frame(flws_mrg %>%
                           group_by(wyr) %>%
                           mutate(tinun_a_cmwy = cumsum(tinun_a)) %>%
                           mutate(inun_a_cmwy = cumsum(inun_a)) %>%
                           mutate(conn_a_cmwy = cumsum(conn_a)) %>%
                           mutate(dconn_a_cmwy = cumsum(dconn_a))) # %>%
  flws_mrg <- data.frame(flws_mrg %>%
                           group_by(wyr) %>%
                           mutate(salwua_cmwy = cumsum(salwua)))

  return(flws_mrg)

}
