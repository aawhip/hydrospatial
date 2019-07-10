#' Function for hydrospatial application of seasonal timing window
#'
#' This function accepts
#'
#' @details Input rasters are in the  Rasters are written to
#' file in a 'rsdur0' directory within 'outdir'.
#'
#' @param rs_dayinunwgt Raster stack or brick of inundation day as from 'hsa_duration'
#' @param mo_start Threshold start month
#' @param mo_end Threshold number of days for long duration inundation
#' @param fdf Flows data frame for water year in format of 'utils_hsaflws' function
#' @param outdir Directory for writing rasters to file
#' @export
#' @return Flood events data frame for water year with duration metrics filled in.
#' Writes the duration rasters to file in the outdir.

hsa_timing <- function(rs_dayinunwgt, mo_start, mo_end, fdf, outdir) {
  # Change values to zero that are earlier than Nov 1 and later than June 30 - I should also be able to wrap this into a previous operation to not create separate rasters
    mnths <- month(fdf$dt)

  fun_time <- function(x) {
    if(all(is.na(x))){x <- rep(NA, length(x))}
    else {
      x[which(mnths > mo_end & mnths < mo_start)] <- 0
    }
    return(x)
  }

  beginCluster(ncores)
    cl <- getCluster()
    clusterExport(cl, list("mnths","mo_end","mo_start"), envir = environment())
    rs_timewgt <- clusterR(rs_dayinunwgt, calc, args=list(fun=fun_time))
  endCluster()

  writeRaster(rs_timewgt, filename=paste0(outdir,"rshab/rstimewgt_",wy,".grd"), bylayer=T, suffix='numbers', overwrite=T)

}
