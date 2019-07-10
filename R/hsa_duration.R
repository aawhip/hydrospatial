#' Function for hydrospatial analysis of inundation duration
#'
#' This function accepts
#'
#' @details Input rasters are in the 'rsi' format of output from the hsa_extent function,
#' where cells = 1 are inundated and non-inundated cells = NA. Rasters are written to
#' file in a 'rsdur0' directory within 'outdir'.
#'
#' @param rs_i Raster stack or brick with inundated cells = 1
#' @param fdf_e Flood events data frame for water year in format of 'utils_hsaflwsevts'
#' function
#' @param igrp Vector of grouped floods days of length equal to number of rs_i layers,
#' where each unique value is a unique flood event
#' @param wy Water year to add to filenames
#' @param outdir Directory for writing rasters to file
#' @export
#' @return Flood events data frame for water year with duration metrics filled in.
#' Writes the duration rasters to file in the outdir.

hsa_duration <- function(rs_i, fdf_e, igrp, wy, outdir) {

  # Make directory if necessary
  if (!dir.exists(paste0(outdir,"rsdur0"))) {dir.create(paste0(outdir,"rsdur0"))}

  # New RasterStack/Bricks: Duration of inundation within each event - by cell
    rs_dur0 <- stackApply(rs_i, indices=igrp, fun=sum)
    rs_dur <- reclassify(rs_dur0, cbind(0,NA)) # Create a duration raster stack that has NA instead of 0

    writeRaster(rs_dur0, filename=paste0(procdir,"rsdur0/rsdur0_",wy,".grd"), bylayer=T, suffix='numbers', overwrite=TRUE)

  # Add duration summaries to events flows data frame
    fdf_e$dur_max <- cellStats(rs_dur, max) # Max duration across inundated cells
    fdf_e$dur_m <- cellStats(rs_dur, mean) # Mean duration across inundated cells
    fdf_e$dur_sd <- cellStats(rs_dur, sd) # SD of duration across inundated cells
    fdf_e$dur_cv <- fdf_e$dur_sd/fdf_e$dur_m*100 # CV of duration across inundated cells

  return(fdf_e)

}
