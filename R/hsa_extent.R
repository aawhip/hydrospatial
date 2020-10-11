#' Hydrospatial analysis of inundation extent with depth and velocity criteria
#'
#' Accepts depth and velocity rasters (from \code{predrast_interp}) and
#' associated flows data frame for a given water year and calculates three sets
#' of binary rasters, indicating whether cells are inundated, whether they meet
#' the depth thresholds, and whether they meet the velocity threshold. The area
#' for each raster is determined and added to the flows data frame.
#'
#' @details Rasters are written to file in a 'rsi' directory within 'outdir'.
#'
#' @param rs_d Raster stack or brick of depth
#' @param rs_v Raster stack or brick of velocity
#' @param fdf Flows data frame for water year in format of 'utils_hsaflws'
#'   function
#' @param dmin Minimum depth threshold for raster calculation
#' @param dmax Maximum depth threshold for raster calculation
#' @param vmax Maximum velocity threshold for raster calculation
#' @param wy Water year to add to filenames
#' @param cres Resolution of cell (in units squared)
#' @param aconv Conversion factor for calculating area
#' @param fp_a Area of study site
#' @param ncor Number of cores for parallel processing
#' @param outdir Directory for writing rasters to file
#' @export
#' @return Flows data frame with inundated area metrics filled in. Writes the
#'   three sets of rasters to file in the outdir.

hsa_extent <- function(rs_d, rs_v, fdf, dmin, dmax, vmax, wy, cres, aconv, fp_a, ncor, outdir) {

  # Make directory if necessary
    if (!dir.exists(paste0(outdir,"rsi"))) {dir.create(paste0(outdir,"rsi"))}

  # New RasterStack/Bricks: Changes areas greater than the depth threshold to 1, all else to 0, NAs left as NA

    f1 <- function(x) {x > 0} # Is the inundated depth greater than 0?
    f2 <- function(x) {x >= dmin & x <= dmax} # Does inundation depth meet both min and max thresholds?
    f3 <- function(x) {x <= vmax} # Does velocity meet the max threshold?

    beginCluster(ncor)
      cl <- getCluster()
      clusterExport(cl,list("dmin","dmax","vmax"), envir = environment())
      rs_ti0 <- clusterR(rs_d, calc, args=list(fun=f1)) # get a set that's just whether inundated or not
      rs_i0_d <- clusterR(rs_d, calc, args=list(fun=f2))
      rs_i0_v <- clusterR(rs_v, calc, args=list(fun=f3))
      rs_i0 <- rs_i0_d*rs_i0_v
      rs_i <- reclassify(rs_i0, cbind(0,NA))# Create rasters with 0s to NA
    endCluster()

    writeRaster(rs_ti0, filename=paste0(outdir,"rsi/rsti0_",wy,".grd"), bylayer=TRUE, suffix='numbers', overwrite=TRUE)
    writeRaster(rs_i0, filename=paste0(outdir,"rsi/rsi0_",wy,".grd"), bylayer=TRUE, suffix='numbers', overwrite=TRUE)
    writeRaster(rs_i, filename=paste0(outdir,"rsi/rsi_",wy,".grd"), bylayer=TRUE, suffix='numbers', overwrite=TRUE)

  # Add total inundated area
    fdf$tinun_a <- cellStats(rs_ti0, sum)*cres*aconv
  # Add inundated area meeting thresholds to flows data frame for analysis
    fdf$inun_a <- cellStats(rs_i0, sum)*cres*aconv
  # Add percent inundated area to flows data frame for analysis
    fdf$pinun_a <- 100*(fdf$inun_a/fp_a)

  return(fdf)

}
