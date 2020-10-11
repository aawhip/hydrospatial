#' Hydrospatial application of seasonal timing window
#'
#' Accepts rasters (from \code{hsa_durationwgt}) and applies a timing window
#' where values are assigned to zero if they fall outside the window.
#'
#' @details Input rasters from 'rshab' directory. Rasters are written to file in
#'   'rshab' directory within 'outdir'.
#'
#' @param rs_dayinunwgt Raster stack or brick of inundation day as from
#'   'hsa_duration'
#' @param mo_start Threshold start month
#' @param mo_end Threshold number of days for long duration inundation
#' @param fdf Flows data frame for water year in format of 'utils_hsaflws'
#'   function
#' @param wy Water year to add to filenames
#' @param cres Resolution of cell (in units squared)
#' @param aconv Conversion factor for calculating area
#' @param ncor Number of cores for parallel processing
#' @param outdir Directory for writing rasters to file
#' @export
#' @return Flows data frame for water year with suitable habitat area (wua,
#'   weighted usable area) and hydraulic habitat suitability (hhs) filled in.
#'   Writes the suitability rasters with the final timing weighting to file in
#'   the outdir.

hsa_timing <- function(rs_dayinunwgt, mo_start, mo_end, fdf, wy, cres, aconv, ncor, outdir) {
  # Change values to zero that are within the timing window
    mnths <- month(fdf$dt)

  fun_time <- function(x) {
    if(all(is.na(x))){x <- rep(NA, length(x))}
    else {
      x[which(mnths > mo_end & mnths < mo_start)] <- 0
    }
    return(x)
  }

  beginCluster(ncor)
    cl <- getCluster()
    clusterExport(cl, list("mnths","mo_end","mo_start"), envir = environment())
    rs_timewgt <- clusterR(rs_dayinunwgt, calc, args=list(fun=fun_time))
  endCluster()

  # Compute WUA at each timestep
  fdf$salwua <- cellStats(rs_timewgt, sum)*cres*aconv
  fdf$salwua <- fdf$salwua*fdf$areaminreq
  # Compute HHS at each timestep - add to flws data frame
  fdf$salhhs <- fdf$salwua/fdf$tinun_a

  writeRaster(rs_timewgt, filename=paste0(outdir,"rshab/rshabsuit_",wy,".grd"), bylayer=T, suffix='numbers', overwrite=T)

  return(fdf)

}
