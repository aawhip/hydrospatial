#' Hydrospatial analysis of inundation duration
#'
#' Accepts rasters indicating inundated cells (from \code{hsa_extent}),
#' connected and disconnected cells (from \code{hsa_connectivity}), and
#' consecutive days of inundation (from \code{hsa_freq}) to calculate 1) flood
#' event rasters of inundation duration, inundation duration of connected cells,
#' and inundation duration of disconnected cells, and 2) daily rasters of
#' duration of inundation at a cell.
#'
#' @details Input rasters are from 'rsi' directory, where cells = 1 are
#'   inundated and non-inundated cells = NA. Daily rasters of duration (with
#'   filenames beginning in 'rsdayinun0') represent the number of days inundated
#'   up to that day, and cells are checked such that they do not get
#'   disconnected and stay disconnected (beyond seven days). Rasters are written
#'   to file in a 'rsdur0' directory within 'outdir'.
#'
#' @param rs_i Raster stack or brick with inundated cells = 1
#' @param rs_c Raster stack or brick with inundated and connected cells = 1 as
#'   from 'hsa_connectivity'
#' @param rs_dc Raster stack or brick with inundated and disconnected cells = 1
#'   as from 'hsa_connectivity'
#' @param rs_noinun Raster stack or brick with inundation groupings as from
#'   'hsa_freq'
#' @param fdf_e Flood events data frame for water year in format of
#'   'utils_hsaflwsevts' function
#' @param fdf Flows data frame for water year in format of 'utils_hsaflws'
#'   function
#' @param igrp Vector of grouped floods days of length equal to number of rs_i
#'   layers, where each unique value is a unique flood event
#' @param wy Water year to add to filenames
#' @param ncor Number of cores for parallel processing
#' @param outdir Directory for writing rasters to file
#' @export
#' @return Flood events data frame for water year with duration metrics filled
#'   in. Writes the duration rasters to file in the outdir.

hsa_duration <- function(rs_i, rs_c, rs_dc, rs_noinun, fdf_e, fdf, igrp, wy, ncor, outdir) {

  # Make directories if necessary
  if (!dir.exists(paste0(outdir,"rsdur0"))) {dir.create(paste0(outdir,"rsdur0"))}
  if (!dir.exists(paste0(outdir,"rscdur0"))) {dir.create(paste0(outdir,"rscdur0"))}
  if (!dir.exists(paste0(outdir,"rsdcdur0"))) {dir.create(paste0(outdir,"rsdcdur0"))}
  if (!dir.exists(paste0(outdir,"rsdayinun0"))) {dir.create(paste0(outdir,"rsdayinun0"))}

  # New RasterStack/Bricks: Duration of inundation within each event - by cell
    rs_dur0 <- stackApply(rs_i, indices=igrp, fun=sum)
    rs_dur <- reclassify(rs_dur0, cbind(0, NA)) # Create a duration raster stack that has NA instead of 0
    rs_cdur0 <- stackApply(rs_c, indices=igrp, fun=sum)  # write to file?
    rs_cdur <- reclassify(rs_cdur0, cbind(0, NA)) # write to file?
    rs_dcdur0 <- stackApply(rs_dc, indices=igrp, fun=sum) # write to file?
    rs_dcdur <- reclassify(rs_dcdur0, cbind(0, NA)) # write to file?

    writeRaster(rs_dur0, filename=paste0(outdir,"rsdur0/rsdur0_",wy,".grd"), bylayer=T, suffix='numbers', overwrite=TRUE)
    writeRaster(rs_cdur0, filename=paste0(outdir,"rscdur0/rscdur0_",wy,".grd"), bylayer=T, suffix='numbers', overwrite=TRUE)
    writeRaster(rs_dcdur0, filename=paste0(outdir,"rsdcdur0/rsdcdur0_",wy,".grd"), bylayer=T, suffix='numbers', overwrite=TRUE)

  # Add duration summaries to events flows data frame
    fdf_e$dur_max <- cellStats(rs_dur, 'max') # Max duration across inundated cells
    fdf_e$dur_m <- cellStats(rs_dur, 'mean') # Mean duration across inundated cells
    fdf_e$dur_sd <- cellStats(rs_dur, 'sd') # SD of duration across inundated cells
    fdf_e$dur_cv <- fdf_e$dur_sd/fdf_e$dur_m*100 # CV of duration across inundated cells
    fdf_e$cdur_m <- cellStats(rs_cdur, 'mean') # Mean connected duration across inundated cells
    fdf_e$dcdur_m <- cellStats(rs_dcdur, 'mean') # Mean disconnected duration across inundated cells
    fdf_e$cdur_sd <- cellStats(rs_cdur, 'sd') # SD of connected duration across inundated cells
    fdf_e$dcdur_sd <- cellStats(rs_dcdur, 'sd') # SD of disconnected duration across inundated cells
    fdf_e$cdur_cv <- fdf_e$cdur_sd/fdf_e$cdur_m*100 # CV of connected duration across inundated cells
    fdf_e$dcdur_cv <- fdf_e$dcdur_sd/fdf_e$dcdur_m*100 # CV of disconnected duration across inundated cells

  # Duration of inundation at a cell that doesn't get disconnected and stay disconnected
    # Consecutive days groups
      cnc <- cumsum(!c(TRUE, diff(fdf$dt) == 1))

    # Function to get 1s for days to count in, and 0s and NAs for days to leave out - to use in calc with rs.c (where NA is dry, 0 is disconnected and 1 is connected)
      fun_c <- function(x) { # x is the timeseries for the cell
        if(all(is.na(x))){x<-rep(NA,length(x))} # if the cell over time is all NA, then give back all NAs
        if (!all(is.na(x))) {
          # Set days after 7 disconnected days to NA
          for (i in 1:length(x)) {
            if (!is.na(x[i])&x[i]==0) {
              j <- 1
              while (!is.na(x[i+j])&x[i+j]==0&cnc[i]==cnc[i+j]&j<7) {
                j <- j+1
              }
              while (!is.na(x[i+j])&x[i+j]==0&cnc[i]==cnc[i+j]&j>=7) {
                x[i+j] <- NA
                j <- j+1
              }
            }
          }

          # Assigned reconnected days to 1
          for (i in 1:length(x)) { # For each day
            if (is.na(x[i])){x[i]<-NA} else if (x[i]==0) { # If the day is dry, then leave it NA, if it's connected, then break and go to the next day, if it's disconnected, then
              if (i!=1) { # if we're on the first day and it's disconnected, then break out and leave as 0
                j <- 1
                while (!is.na(x[i-j])&x[i]!=1&cnc[i]==cnc[i-j]) {# If the day before isn't dry, and that day not reassigned to 1, and the cnc group and the cnc group of the day before are equal and we're not looking back more than 7 days
                  if (x[i-j]==1) { # Is connected, then can look ahead to see if should get changed to 1
                    l<-1
                    while (!is.na(x[i+l])&x[i]!=1&cnc[i]==cnc[i+l]) { # while the next day is not NA, still in the same consecutive days group and still not reassigned to 1 and we're not looking forward more than 7 days
                      if (x[i+l]==1){x[i]<-1} # if the following day is connected, then change to wet
                      l<-l+1 # add 1 to l
                    }
                  }
                  if (exists("l")) {
                    if (is.na(x[i+l])|cnc[i]!=cnc[i+l]) {break}
                  }
                  j <- j+1
                  if (i==j) {break}
                }
              }
            }
          }
        }
        return(x)
      }

    beginCluster(ncor)
      cl <- getCluster()
      clusterExport(cl, list("fdf","cnc"), envir = environment())
      rs_cinun0 <- clusterR(rs_c, calc, args=list(fun=fun_c))
    endCluster()

    # Multiply the raster stack of days to keep (as 1) with the raster stack of inundation groupings
      rs_nocinun0 <- overlay(rs_cinun0, rs_noinun, fun=function(x,y) x*y)
      rs_nocinun <- reclassify(rs_nocinun0, cbind(0,NA))

    # Function to get the day of inundation for each group
      fun_inunday <- function(x) {
        if(all(is.na(x))){x <- rep(NA,length(x))}
        else {
          lgrps <- unique(x)[!is.na(unique(x))] # the inundation grouping numbers for the cell, without NA in the list
          y <- x
          for (i in 1:length(lgrps)) {
            x[which(y==lgrps[i])] <- seq(1:sum(y==lgrps[i],na.rm=TRUE))
          }
        }
        return(x)
      }

    beginCluster(ncor)
      rs_dayinun0 <- clusterR(rs_nocinun, calc, args=list(fun=fun_inunday))
    endCluster()

    writeRaster(rs_dayinun0, filename=paste0(outdir,"rsdayinun0/rsdayinun0_",wy,".grd"), bylayer=T, suffix='numbers', overwrite=T)

  return(fdf_e)

}
