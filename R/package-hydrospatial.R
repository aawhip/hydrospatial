#' hydrospatial: A package for characterizing floodplain inundation and habitat
#' in space and time
#'
#' An R-package for spatial and temporal analysis of floodplain inundation
#' characteristics and application of habitat suitability criteria.
#' It takes as input raster data of depth and velocity (e.g., from 2D hydrodynamic
#' modeling) at known flows and a corresponding daily flow time series.
#' Daily rasters of estimated depth and velocity are generated from
#' inputs using spatially resolved piece-wise linear interpolation.
#' Hydrospatial analysis functions calculate a range of physical metrics using
#' the raster time series. Habitat suitability criteria applied to the
#' computed physical metrics estimate spatially resolved daily cell suitability,
#' allowing for computation of daily weighted usable area. Statistical and
#' graphical summary of these metrics in space and time.
#'
#' The hydrospatial package provides four categories of functions:
#' predrast, hsa, utility, and vis
#'
#' @section predictrast functions:
#' The predictrast functions
#'
#' @docType package
#' @name hydrospatial
#'
NULL
