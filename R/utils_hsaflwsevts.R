#' Prepare flood events table for hydrospatial analysis
#'
#' Prepares the flood events table that will be used in hydrospatial analysis,
#' taking as input the flows table from the 'utils_hsaflws' function.
#'
#' @param d Data frame in format of 'utils_hsaflws' function
#' @param sc Scenario name
#' @export
#' @return Flood events data frame as input for hydrospatial analysis

utils_hsaflwsevts <- function(d, sc){

    d$dt <- ymd(d$dt)
    d$sc <- sc
    d$jd <- yday(d$dt) # jd=Julian day
    d$limb <- as.character(d$limb)

  # One row per flood event
    flws_e <- d[NA,]
    flws_e$nday <- NA
    for (i in (1:length(unique(d$event_no)))) {
      evntno <- unique(d$event_no)[i]
      flws_e[i,] <- d[which(d$event_no==evntno)[1],]
      flws_e$nday[i] <- length(which(d$event_no==evntno))
    }
    flws_e <- na.omit(flws_e) # this takes out the extra rows, but also the 2017 rows that have NA in the ft field (if they're still there)
    row.names(flws_e) <- unique(flws_e$event_no)

  # Add empty columns
    flws_e$dur_max <- NA; flws_e$dur_m <- NA; flws_e$dur_sd <- NA; flws_e$dur_cv <- NA
    flws_e$cdur_m <- NA; flws_e$cdur_sd <- NA; flws_e$cdur_cv <- NA
    flws_e$dcdur_m <- NA; flws_e$dcdur_sd <- NA; flws_e$dcdur_cv <- NA

  return(flws_e)

}
