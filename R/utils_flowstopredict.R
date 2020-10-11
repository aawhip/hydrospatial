#' Format flows for raster prediction
#'
#' Identifies flow days for hydrospatial analysis, to be used as input for
#' raster prediction
#'
#' @details It takes a formatted flow time series and flood day flow time series
#'   from hydrospatial 'utils_flowformat' and 'utils_floodid' functions,
#'   respectively.
#'
#' @param d A daily flow time series data frame
#' @param d_flds Flood days data frame
#' @param thr Integer flow threshold to identify flood days
#' @param postfld Days post-flood to retain in dataset. Default is 7.
#' @param outdir Directory for writing flows to file
#' @importFrom dplyr select left_join
#' @export
#' @return Flows data frame as input for raster prediction, written to file as
#'   "flows_topred_full.csv"

utils_flowstopredict <- function(d, d_flds, thr, postfld = 7, outdir){

  # Keep above threshold and within the set days of end of flood
    d$flw_k <- NA
    if (d$flw[2]>thr) {
      d$flw_k[1] <- d$flw[1]
    } else {d$flw_k[1] <- NA}
    for (i in (1:(nrow(d)-1))) {
      if (d$flw[i+1]>=thr) {
        d$flw_k[i+1] <- d$flw[i+1]
      } else if (d$flw[i+1]<thr&i>(postfld+1)) {
        if (sum(d$flw[(i-(postfld-1)):i]>thr)>0) {
          d$flw_k[i+1] <- d$flw[i+1]
        }
      } else {d$flw_k[i+1] <- NA}
    }
    d <- na.omit(d) # Keep the days to predict

  # Join
  d_sel <- select(d_flds,c(dt,event_no,fday))
  d_full <- left_join(d,d_sel, by="dt")

  # fill in the NAs as needed
  for (i in (1:nrow(d_full))) {
    if (is.na(d_full$event_no[i])) {
      d_full$event_no[i] <- d_full$event_no[i-1]
      d_full$fday[i] <- d_full$fday[i-1]+1
    }
  }

  # Export flows tables
  write.csv(d_full, file=paste0(outdir,"flows_topred_full.csv"), row.names=FALSE)

  return(d_full)

}
