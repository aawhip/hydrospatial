#' Create a raster of cell inundation flow thresholds
#'
#' Creates a raster where cell values represent the maximum flow before cells
#' inundate.
#'
#' @details This function uses the raster package 'calc' function, where the
#'   function applied accepts vectors of cell values of depth from a raster
#'   stack and uses the input 'flws' vector that corresponds to each of the cell
#'   values. The function applied within 'calc' returns NA if the cell vector is
#'   all NA or 0 (dry cell across the raster stack). If there are no cells that
#'   are NA or 0 (cells is always wet across the raster stack), it returns 1.
#'   Otherwise, it uses the maximum (or last) flow associated with a 0 or NA
#'   depth value. Note that the determination of flow thresholds is limited by
#'   the flows provided and their associated rasters. For example, if rasters at
#'   100 cfs and 200 cfs are provided but a cell actually inundates at 150 cfs,
#'   the flow threshold assigned to the cell will be 100 cfs (the last flow
#'   provided where the cell is dry).
#'
#' @param rs_mod Raster stack or brick of depth with each layer corresponding to
#'   the flws vector
#' @param flws Vector of flows corresponding to the raster stack or brick
#' @export
#' @importFrom raster calc
#' @importClassesFrom raster Extent BasicRaster Raster RasterLayer RasterBrick
#'   RasterStack RasterStackBrick
#' @return A single raster layer of inundation threshold flows

predrast_thresholds <- function(rs_mod, flws){

  # Define function to be applied to each vector of cells within 'calc'
  fun_thr <- function(x) {
    if(all(is.na(x))){
    NA
    } else if(all(x==0)){
      NA
      } else if(length(flws[is.na(x)|x==0])==0){
        1
      } else {max(flws[is.na(x)|x==0])}
  }

  # Calculate the raster
  rs_thr <- calc(rs_mod, fun_thr)

  return(rs_thr)

}
