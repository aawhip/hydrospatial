#' Hydrospatial application of inundation duration weight
#'
#' Accepts daily rasters of inundation duration (from \code{hsa_duration}) and
#' applies duration weights.
#'
#' @details Input rasters from 'rsdayinun0' directory. Rasters are written to file in
#'   a 'rshab' directory within 'outdir'.
#'
#' @param rs_dayinun Raster stack or brick of inundation day as from
#'   'hsa_duration'
#' @param durshrt Threshold number of days for short duration inundation
#' @param durlng Threshold number of days for long duration inundation
#' @param durshrt_wgt Weight for short duration inundation
#' @param durlng_wgt Weight for long duration inundation
#' @param durtoolng_wgt Weight for too-long duration inundation
#' @param wy Water year to add to filenames
#' @param ncor Number of cores for parallel processing
#' @param outdir Directory for writing rasters to file
#' @export
#' @return Writes the duration weight rasters to file in the outdir.

hsa_durationwgt <- function(rs_dayinun, durshrt, durlng, durshrt_wgt, durlng_wgt, durtoolng_wgt, wy, ncor, outdir) {

  # Make directory if necessary
  if (!dir.exists(paste0(outdir,"rshab"))) {dir.create(paste0(outdir,"rshab"))}

  # Assign credit for day of inundation (I could do this inside the fun.inunday function)
    fun_daycred <- function(x) {
      if(all(is.na(x))){x<-rep(NA,length(x))}
      else {
        shrt <- which(x<=durshrt); lng <- which(x>durshrt&x<=durlng); toolng <- which(x>durlng)
        if (length(shrt>0)) {x[shrt]<-durshrt_wgt}
        if (length(lng>0)) {x[lng]<-durlng_wgt}
        if (length(toolng>0)) {x[toolng]<-durtoolng_wgt}
      }
      return(x)
    }

    beginCluster(ncor)
      cl <- getCluster()
      clusterExport(cl, list("durshrt","durlng","durshrt_wgt", "durlng_wgt", "durtoolng_wgt"), envir = environment())
      rs_dayinunwgt <- clusterR(rs_dayinun, calc, args=list(fun = fun_daycred))
    endCluster()

  writeRaster(rs_dayinunwgt, filename=paste0(outdir,"rshab/rsdayinun0wgt_",wy,".grd"), bylayer=T, suffix='numbers', overwrite=T)

}
