#' Create interpolated rasters based on a flow time series and rasters at known
#' flows
#'
#' Creates and writes to file a raster corresponding to each record in an input
#' flow time series, applying spatially-resolved piece-wise linear interpolation
#' using rasters at known flows.
#'
#' @details This function accepts a stack of rasters (typically of inundation
#'   depth or velocity, as from 2D hydrodynamic modeling) representing
#'   conditions at known flows, which are used to create an interpolated raster
#'   for each flow in the input flow time series. The input flow time series is
#'   typically daily flow time series.
#'
#'   If the ponding option is set to true, the function determines whether the
#'   flows in the input flow time series are on the rising or falling limb of
#'   the hydrograph and uses the corresponding rasters from the input raster
#'   stack. It also uses the flow threshold raster created by the
#'   'predrast_thresholds' function to determine whether cells should be wet or
#'   dry on the falling limb of a hydrograph based on the high flow column.
#'
#' @param rs_mod Raster stack or brick with each layer corresponding to the flws
#'   vector
#' @param r_ithr Raster layer of flow inundation thresholds of cells, as from
#'   the 'predrast_thresholds' function. Used if 'ponding' is TRUE.
#' @param flws_mod Data frame of flows corresponding to the raster stack or
#'   brick, containing a 'flw' numeric column and a 'limb' attributing flows as
#'   either rising ('r') or falling ('f')
#' @param flws_pred Data frame of flows for which to create interpolated
#'   rasters, composed of a date ('dt') column, flows ('flw') column, a limb
#'   ('limb') column assigning the flow as either on the rising ('r') or falling
#'   ('f') limb of the hydrograph, and a high flow ('hflw') column representing
#'   the recent peak flow to which ponded or disconnected areas correspond (such
#'   as the highest flow within the last seven days).
#' @param ponding Set ponding to TRUE or FALSE. Default value is FALSE.
#' @param sc Character string to attach to the output raster filenames (such as
#'   the name of the model scenario)
#' @param vbl Character string to attach to the output raster filenames (e.g.,
#'   'Depth')
#' @param preddir Directory to which the rasters will be written
#' @export
#' @importFrom raster stack overlay mask writeRaster
#' @importClassesFrom raster Extent BasicRaster Raster RasterLayer RasterBrick
#'   RasterStack RasterStackBrick
#' @return Rasters written to file

predrast_interp <- function(rs_mod, r_ithr, flws_mod, flws_pred, ponding=FALSE, sc, vbl, preddir){

  # Step through for each flow in flws_pred

  for (i in (1:nrow(flws_pred))) {

    flw <- flws_pred$flw[i] # the flow to predict
    hflw <- flws_pred$hflw[i] # the recent high flow peak to consider for thresholding

    if (ponding==FALSE) {
      if (flw%in%flws_mod$flw) {
        r_p <- rs_mod[[which(flws_mod$flw==flw)]]
        r_p[is.na(r_p)] <- 0
      } else {
        inx_bfr <- which(flws_mod$flw==max(flws_mod$flw[(flws_mod$flw<flw)])) # index of the flow before
        inx_aft <- which(flws_mod$flw==min(flws_mod$flw[(flws_mod$flw>flw)])) # index of the flow after
        intrp <- (flw-flws_mod$flw[inx_bfr])/(flws_mod$flw[inx_aft]-flws_mod$flw[inx_bfr]) # the fraction to add for the linear interpolation
        rs <- stack(rs_mod[[inx_bfr]], rs_mod[[inx_aft]])
        rs[is.na(rs)] <- 0
        r_p <- overlay(rs, fun = function(x,y){return(x+(intrp*(y-x)))}) # the interpolation function cell-by-cell for the two prediction rasters
      }

    } else if (ponding==TRUE) {
      if (flws_pred$limb[i]=="r") { # If on the rising limb of hydrograph
        if (flw%in%flws_mod$flw[flws_mod$limb=="r"]) {
          r_p <- rs_mod[[which(flws_mod$flw==flw&flws_mod$limb=="r")]]
          r_p[is.na(r_p)] <- 0
        } else {
          inx_bfr <- which(flws_mod$flw==max(flws_mod$flw[(flws_mod$flw<flw)])&flws_mod$limb=="r") # index of the flow before
          inx_aft <- which(flws_mod$flw==min(flws_mod$flw[(flws_mod$flw>flw)])&flws_mod$limb=="r") # index of the flow after
          intrp <- (flw-flws_mod$flw[inx_bfr])/(flws_mod$flw[inx_aft]-flws_mod$flw[inx_bfr]) # the fraction to add for the linear interpolation
          rs <- stack(rs_mod[[inx_bfr]],rs_mod[[inx_aft]])
          rs[is.na(rs)] <- 0
          r_p <- overlay(rs,fun=function(x,y){return(x+(intrp*(y-x)))}) # the interpolation function cell-by-cell for the two prediction rasters
        }
      } else if (flws_pred$limb[i]=="f") { # If on the falling limb of the hydrograph
        ithr_mask <- r_ithr # create a masking layer from the inundation threshold layer
        ithr_mask[ithr_mask>hflw] <- NA # set the values greater than the flow of interest to NA
        if (flw%in%flws_mod$flw[flws_mod$limb=="f"]) {
          r_p <- rs_mod[[which(flws_mod$flw==flw&flws_mod$limb=="f")]]
          r_p <- mask(r_p,ithr_mask) # makes only the non-NA values in the mask show through in the rasterstack
          r_p[is.na(r_p)] <- 0 # reset the NA values to 0
        } else {
          inx_bfr <- which(flws_mod$flw==max(flws_mod$flw[(flws_mod$flw<flw)])&flws_mod$limb=="f") # index of the flow before
          inx_aft <- which(flws_mod$flw==min(flws_mod$flw[(flws_mod$flw>flw)])&flws_mod$limb=="f") # index of the flow after
          intrp <- (flw-flws_mod$flw[inx_bfr])/(flws_mod$flw[inx_aft]-flws_mod$flw[inx_bfr]) # the fraction to add for the linear interpolation
          rs <- stack(rs_mod[[inx_bfr]],rs_mod[[inx_aft]])
          rs <- mask(rs,ithr_mask) # makes only the non-NA values in the mask show through in the rasterstack
          rs[is.na(rs)] <- 0 # reset the NA values to 0
          r_p <- overlay(rs,fun=function(x,y){return(x+(intrp*(y-x)))}) # the interpolation function cell-by-cell for the two prediction rasters
        }
      }
    }

    r_p[r_p==0] <- NA # Set zeros to NA

    names(r_p) <- paste0(sc,"_",vbl,"_",as.character(flws_pred$dt[i]))

    # Write each raster to file
    writeRaster(r_p, paste0(preddir,names(r_p),".grd"), overwrite=TRUE)

  }
}
