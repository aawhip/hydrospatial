#' Hydrospatial analysis of inundation frequency
#'
#' Accepts a raster stack/brick representing inundated area (from
#' \code{hsa_extent}) and assigns unique values per cell across the stack/brick
#' for each group of consecutive days of inundation.
#'
#' @details Input rasters are in the 'rsi' format of output from
#'   \code{hsa_extent}, where cells = 1 are inundated (meeting depth and
#'   velocity threshold criteria) and non-inundated cells = 0. Rasters are
#'   written to file in a 'rsnoinun' directory within 'outdir'.
#'
#' @param rs_i0 Raster stack or brick with inundated cells = 1
#' @param fdf Flows data frame for water year in format of 'utils_hsaflws'
#'   function
#' @param wy Water year to add to filenames
#' @param ncor Number of cores for parallel processing
#' @param outdir Directory for writing rasters to file
#' @export
#' @return Writes rasters with groupings of inundation to file in the outdir.

hsa_freq <- function(rs_i0, fdf, wy, ncor, outdir) {

  # Make directory if necessary
  if (!dir.exists(paste0(outdir,"rsnoinun"))) {dir.create(paste0(outdir,"rsnoinun"))}

  # Function to get the groupings of inundation
    fun_noinun <- function(x, na.rm=FALSE) {
      if (all(is.na(x))) {x <- rep(NA, length(x))}
      # First check converts 0s to NA, then takes each cell vector and numbers each groupings
      # of non-NA values, adding the consec days groupings makes sure that non-consecutive days
      # aren't counted together
      else {is.na(x) <- !x; x <- c(factor(cumsum(is.na(x))*NA^(is.na(x)))) + cumsum(!c(TRUE, diff(fdf$dt) == 1))}
    }

  # New RasterStack/Bricks: Get a daily raster stack with the groupings of inundation
    beginCluster(ncor)
      cl <- getCluster()
      clusterExport(cl,"fdf", envir = environment())
      rs_noinun <- clusterR(rs_i0, calc, args=list(fun=fun_noinun))
    endCluster()

    writeRaster(rs_noinun, filename=paste0(outdir,"rsnoinun/rsnoinun_",wy,".grd"), bylayer=TRUE, suffix='numbers', overwrite=TRUE)

}
