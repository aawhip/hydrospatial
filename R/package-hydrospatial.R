#' hydrospatial: A package for characterizing floodplain inundation and habitat
#' in space and time
#'
#' An R-package for spatial and temporal analysis of floodplain inundation
#' characteristics and application of habitat suitability criteria. It
#' identifies flood events based on one or more floodplain inundation threshold
#' flows and calculates a set of flood event metrics for a given daily flow time
#' series. It takes as input raster data of depth and velocity (e.g., from 2D
#' hydrodynamic modeling) at known flows and a corresponding daily flow time
#' series. Daily rasters of estimated depth and velocity are generated from
#' inputs using spatially resolved piece-wise linear interpolation. Hydrospatial
#' analysis functions calculate a range of physical metrics using the raster
#' time series. Habitat suitability criteria applied to the computed physical
#' metrics estimate spatially resolved daily cell suitability, allowing for
#' computation of daily weighted usable area. Statistical and graphical summary
#' of these metrics in space and time.
#'
#' @details The hydrospatial package provides four categories of functions:
#'   predrast, hsa, utils, and vis
#'
#' @section predrast functions: The predict rasters functions generate daily
#'   rasters of depth and velocity for use by the hydrospatial analysis
#'   functions. \code{predrast_interp} uses input daily flow time series and
#'   rasters representing depth and velocity at known flows to establish (using
#'   spatially-resolved piece-wise linear interpolation) estimated depth and
#'   velocity rasters for each day in the daily flow time series.
#'   \code{predrast_thresholds} creates a raster where cell values represent the
#'   flow above which the cell is inundated.
#'
#' @section hsa functions: The hydrospatial analysis functions evaluate
#'   inundation patterns using a set of metrics and user input thresholds.
#'   Analysis is conducted at the raster cell scale for each flood day of the
#'   user input daily flow time series.
#'
#' @section utils functions: The utility functions serve a range of needs for
#'   the main analysis, including formatting the input daily flow time series,
#'   calculating flood event metrics, summary functions, and various helper
#'   functions.
#'
#' @docType package
#' @name hydrospatial
#'
NULL
