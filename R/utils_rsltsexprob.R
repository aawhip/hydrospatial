#' Set exceedance probability table from annual time series
#'
#' Prepares the exceedance probability table from time series results table for
#' use in visualizing.
#'
#' @param d Data frame in format used for 'hsa' functions
#' @param tmsrs Data frame of annual time series from 'utils_areaday' function
#' @param sc Scenario name to add as a column
#' @importFrom lubridate ymd yday
#' @importFrom dplyr left_join
#' @export
#' @return Flows data frame as input for hydrospatial analysis

utils_rsltsexprob <- function(d, tmsrs, sc){

  # Determine missing years
    yrs <- seq(min(d$wyr),max(d$wyr))
    myrs <- yrs[which(!(yrs%in%unique(d$wyr)))]
    zrs <- rep(0,length(myrs))

  # Generate table to add to for exceedance probability
    ExP_all <- data.frame(wyr=c(unique(d$wyr),myrs), sc=sc)

  # Exceedance prob
    ExP_all$sum_a <- c(tmsrs$sum_a)
    rnk <- rank(-ExP_all$sum_a, ties.method="min")
    ExP_all$ep_sum_a <- 100*(rnk/(length(yrs)+1))

  return(ExP_all)

}
