#' Hydrospatial analysis of hydrologic connectivity
#'
#' Accepts binary daily rasters with inundated (1) and dry (0) cells (from
#' \code{hsa_extent}). For each raster, patches of inundated areas are
#' identified that intersect with the shapefile ('connpoly') that represents
#' area that should be considered as connected to the river.
#' Connected/disconnected inundated area rasters are written to file and
#' connected/disconnected area is calculated.
#'
#' @details Input rs_ti0 rasters (inundated cells prior to threshold
#'   application) from the 'rsi' directory (could also use rs_i0, which
#'   represented inundated areas that meet depth and velocity threshold
#'   criteria). Rasters are written to file in directories 'rsc' and 'rsdc'
#'   within 'outdir'.
#'
#' @param rs_ti0 Raster stack or brick with inundated cells = 1
#' @param flws Flows data frame for water year in format of 'utils_hsaflws'
#'   function
#' @param wy Water year to add to filenames
#' @param cres Resolution of cell (in units squared)
#' @param aconv Conversion factor for calculating area
#' @param connpoly Shapefile covering areas that count as connected to the river
#'   low-flow channel
#' @param outdir Directory for writing rasters to file
#' @export
#' @return Flows data frame with metrics filled in. Writes rasters with
#'   groupings of inundation to file in the outdir.

hsa_connectivity <- function(rs_ti0, fdf, wy, cres, aconv, connpoly, outdir) {

  # Make dir if necessary
  if (!dir.exists(paste0(outdir,"rsc"))) {dir.create(paste0(outdir,"rsc"))}
  if (!dir.exists(paste0(outdir,"rsdc"))) {dir.create(paste0(outdir,"rsdc"))}

  # Generate raster stack where 1s are connected to river, 0s are disconnected, and NA are not inundated
    rs_c <- stack()

    for (i in 1:nlayers(rs_ti0)) {
      rs_clmp <- clump(rs_ti0[[i]]) # Queen's case for adjacency; rs.time[[24]]
      # Set all clumps below a certain size to NA
      clmpfreq <- as.data.frame(freq(rs_clmp)) # clump freq table
      exclID <- clmpfreq$value[which(clmpfreq$count==1)] # count is the size (# of cells)
      rs_clmp[raster::match(rs_clmp, exclID)] <- NA # set the small patches to NA in the clump raster layer
      ext <- extract(rs_clmp, connpoly, df=TRUE) # returns big dataframe of the clump #s that intersect the line, intersect() uses the extent...
      conn <- unique(na.omit(ext[, 2])) # get the clump numbers that intersect w the edge
      nconn <- raster::unique(na.omit(rs_clmp))[which(!(raster::unique(na.omit(rs_clmp)) %in% conn))] # get the clump numbers that don't intersect w the edge
      conn_df <- data.frame(ID=c(conn, nconn),nval=c(rep(1, length(conn)), rep(0, length(nconn)))) # data frame of from-to values
      rs_clmp <- subs(rs_clmp, conn_df) # replace from with to values, not that quick; conn.stk[[i]]
      rs_c <- stack(rs_c, rs_clmp) # write to file
    }

    writeRaster(rs_c, filename=paste0(outdir,"rsc/rsc_",wy,".grd"), bylayer=T, suffix='numbers', overwrite=T)

  # Make disconnectivity stack
    rs_dc <- reclassify(rs_c,matrix(c(0,1,1,0),nrow=2))
    writeRaster(rs_dc, filename=paste0(outdir,"rsdc/rsdc_",wy,".grd"), bylayer=T, suffix='numbers', overwrite=T)

    # Add connected inundated area to flows data frame for analysis
    fdf$conn_a <- cellStats(rs_c, sum)*cres*aconv # do I need to use in quotes for large files?
    # Add disconnected inundated area to flows data frame for analysis
    fdf$dconn_a <- cellStats(rs_dc, sum)*cres*aconv

    return(fdf)

}
