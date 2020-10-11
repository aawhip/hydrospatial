#' Function for applying minimum area threshold
#'
#' This function accepts the flows data frame and calculates whether the area
#' threshold is exceeded at some point during a flood event.
#'
#' @details If the area requirement is met for at least one day during a given
#' flood event, all days are given a value of one, otherwise the days are set to zero.
#'
#' @param fdf Flows data frame for water year in format of 'utils_hsaflws' function
#' @param area_thr Minimum area threshold
#' @export
#' @return Flows data frame with areaminreq field filled in.

hsa_areathreshold <- function(fdf, area_thr) {

  # Check that area during the flood event is at over the area threshold for at least 1 day
  for (i in unique(fdf$event_no)) { # for each flood event index of that year
    indx <- which(fdf$event_no==i) # indices of the days associated with that event
    if (sum(fdf$tinun_a[indx] >= area_thr, na.rm = TRUE) < 1) {
      fdf$areaminreq[indx] <- 0
    } else {
      fdf$areaminreq[indx] <- 1
      }
  }

  return(fdf)

}
