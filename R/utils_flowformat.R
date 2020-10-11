#' Format daily flow time series
#'
#' Accepts a daily flow time series and returns a formatted daily flow time
#' series for use in other hydrospatial functions
#'
#' @details Water years are defined as starting on October 1. Cumulative and
#'   annual volume are calculated in the units of the input flow (e.g., cubic
#'   meters per second becomes cubic meters). Days on the rising limb of the
#'   hydrograph are defined as days where flow is greater than or equal to the
#'   flow over the last seven days.
#'
#' @param d A daily flow time series data frame including a date column ('dt')
#'   in mdy format and flow column ('flw'). There should be no missing days.
#' @importFrom lubridate mdy year month
#' @importFrom dplyr group_by summarize
#' @importFrom magrittr %>%
#' @export
#' @return Formatted daily flows data frame with water year ('wyr'), water year
#'   day ('wyrd'), julian day ('jd'), cumulative flow ('cflw'), annual flow
#'   volume ('anvol'), quintiles of annual flow ('qntl'), high flow of the last
#'   seven days ('hflw'), and rising or falling limb assigned ('limb')

utils_flowformat <- function(d){

  # Set date
    d$dt <- mdy(d$dt)

  # Add water year
    d$wyr <- ifelse(month(d$dt)>=10,year(d$dt)+1,year(d$dt))

  # Add water year day, and cumulative sum of flow
    for (i in min(d$wyr):max(d$wyr)) {
      d$wyrd[d$wyr==i] <- seq(1:sum(d$wyr==i))
      d$cflw[d$wyr==i] <- cumsum(d$flw[d$wyr==i])*3600*24
    }

  # Add julian day
    d$jd <- yday(d$dt)

  # Add annual flow volume and 20% quantiles
    anvol <- data.frame(d %>%
      group_by(wyr) %>%
      summarize(vol = sum(flw)*3600*24))

    # Add 20% quantiles
      qntl <- quantile(anvol$vol,probs = seq(0,1,0.2))
      qntln <- c(0.2,0.4,0.6,0.8,1)
      # For each quantile (above the 0 percentile), find the years with flow less than the one ahead and greater than the one before
      anvol$qntl <- NA
      for (i in 1:5) {
        anvol$qntl[which(anvol$vol<=qntl[i+1]&anvol$vol>=qntl[i])] <- qntln[i]
      }

    for (i in min(d$wyr):max(d$wyr)) {
      d$anvol[d$wyr==i] <- anvol$vol[anvol$wyr==i]
      d$qntl[d$wyr==i] <- anvol$qntl[anvol$wyr==i]
    }

  # Set the highest flow within the last 7 days
    d$hflw <- NA
    # address the first 7 days
    d$hflw[1] <- d$flw[1]; d$hflw[2] <- max(d$flw[1:2])
    d$hflw[3] <- max(d$flw[1:3]);d$hflw[4] <- max(d$flw[1:4])
    d$hflw[5] <- max(d$flw[1:5]);d$hflw[6] <- max(d$flw[1:6])
    d$hflw[7] <- max(d$flw[1:7])
    # iterate
    for (i in (8:nrow(d))) {
      d$hflw[i] <- max(d$flw[(i-7):i])
    }

  # Assign days as rising or falling
    d <- transform(d, limb = ifelse(flw>=hflw, "r", "f")) # defined as "r" if the flw of the day is higher than flow in the previous 7 days

  return(d)

}

