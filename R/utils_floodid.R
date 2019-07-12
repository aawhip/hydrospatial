#' Function to identify flood days and events
#'
#' This function accepts a daily flow time series and a vector of flood
#' thresholds for identifying flood days.
#'
#' @details A daily flow time series data frame is required (as from
#' utils_flowformat function), including the following columns: dt (Date),
#' wyr (water year), wyrd (water year day), flw (flow), cflow (cumulative
#' flow to date). Using a single value or vector of flood thresholds, flood
#' days are identified and flood event metrics are determined.
#'
#' @param d Data frame of daily flow time series
#' @param Q Vector of flow thresholds to use to id flood days
#' @param outdir Directory for writing flows to file
#' @importFrom lubridate month day year mdy ymd
#' @export
#' @return List of 1) floods, 2) filenames, 3) daily flows with flood id.
#' Writes to file daily and flood event csvs.

utils_floodid <- function(d, Q, outdir) {

  # Set variables
  f_data <- vector("list",length(Q)) # Create a list to store data from each threshold value
  f_filenames <- vector("character", length(Q))
  daily_data <- vector("list",length(Q)) # Create a list to store daily data from each threshold value

  # For each threshold value, pull out the flood events associated with it
  for (i in 1:length(Q)) {
    # Initiate the main flood data frame and variables to be summarized
    names <- c("wyr","start","end","start_wym","end_wym","start_d","end_d","cent_d","pk_d","no_d","no_pks","flw_pk","flw_mean","vol",
               "frac_v_cent","r_rising","r_falling","cflw","an_vol")
    # wyr = water year, start = start date with origin = "1970-01-01", end = end date with origin = "1970-01-01", start_wym = numeric wy mo of
    # start, end_wym = numeric wy mo of end, start_d = water year day at start of flood, end_d = water year day at end of flood, cent_d = water
    # year day at the vol centroid of flood, pk_d = water year day at the peak flow of event, no_d = duration in days, no_pks = number of peaks,
    # cms_pk = peak daily flow, cms_mean = mean daily flow, v_maf = volume of event, frac_v_cent = days to >0.5 total event vol divided by total
    # days, r_rising = -max rising rate, r_falling = max falling rate, ccms = cumulative vol flow of wyr to start of event

    # Initiate the table to house the flood event number and flow for each day of flood
    floods <- as.data.frame(matrix(0,nrow=nrow(d),ncol=length(names)))
    colnames(floods) <- names
    names_d <- c("dt","wyr","event_no","day","flw","wyrd","mo","yr","fday","an_vol","hflw","limb")
    daily <- as.data.frame(matrix(0,nrow=nrow(d),ncol=length(names_d))) # Initiate a data frame that will be trimmed later
    colnames(daily) <- names_d
    daily$dt <- as.Date(as.character(NA))
    daily$limb <- as.character(daily$limb)

    threshold <- Q[i]
    notflood <- 1 # Start flooding boolean as not flooding
    flooddays <- 0 # Start number of flood days as 0
    meanflow <- 0 # Start mean flow (cms) as 0
    volume <- 0 # Start total flow volume (maf) as 0
    vol_v <- numeric(length=366) # Set an empty vector to hold accumulating volume for frac_v_cent
    r_rising <- 0 # Start the rising difference as 0
    r_falling <- 0 # Start the falling difference as 0
    pk_cnt <- 0 # Counter for number of peaks in an event
    trigger_pk <- 0 # Trigger for a peak
    trigger_vly <- 1 # Trigger for a valley
    k <- 0 # Counter for flood events
    dc <- 0 # Counter for daily dataset

    for (j in 1:nrow(d)) {
      # set next day flow and if at end of time series, set to same day
      if (j==nrow(d)) {nxtflw <- d$flw[j]} else {nxtflw <- d$flw[j+1]}

      # if flow is below threshold or it's the end of the water year
      if (d$flw[j] < threshold|(month(d$dt[j])==9&day(d$dt[j])==30)) {
        if (notflood == 0) { # if it's flooding
          floods$end[k] <- d$dt[j] # Enter the end date of the flood
          floods$end_wym[k] <- ifelse(month(d$dt[j])>=10,month(d$dt[j])-9,month(d$dt[j])+3) # Enter the wy month of the end of the flood event
          floods$end_d[k] <- d$wyrd[j] # Enter the water year day of the end of the flood
          floods$no_d[k] <- flooddays # Enter the number of days of the flood event
          # Enter number of peaks in the flood event
          if (flooddays == 1) {
            floods$no_pks[k] <- 1
          }
          else {
            floods$no_pks[k] <- pk_cnt
          }
          floods$flw_mean[k] <- meanflow/flooddays # Enter the mean daily flow (cms)
          floods$vol[k] <- volume # Enter the flood volume (maf)
          vol_v <- vol_v[vol_v!=0] # Remove zeros from vector and proceed with determining the frac_v_cent
          for (m in 1:length(vol_v)) {
            if (vol_v[m] > (0.5*volume)) {
              floods$frac_v_cent[k] <- m/flooddays
              floods$cent_d[k] <- m+floods$start_d[k]-1
              break
            }
          }
          floods$r_rising[k] <- r_rising # Enter the largest differece in flow on rising limb
          floods$r_falling[k] <- r_falling # Enter the largest difference in flow on falling limb
          flooddays <- 0 # Reset flood days
          meanflow <- 0 # Reset meanflow
          volume <- 0 # Reset volume
          vol_v <- numeric(length=366) # Reset the empty vector for accumulating volume
          r_rising <- 0 # Reset rising difference
          r_falling <- 0 # Reset falling difference
          pk_cnt <- 0 # Reset counter for number of peaks in an event
          trigger_pk <- 0 # Reset trigger for a peak
          trigger_vly <- 1 # Reset trigger for a valley
          notflood <- 1 # Set condition to not flooding
        }
      }
      # when it's equal to or above the threshold
      else {
        if (notflood == 1) { # if it just started flooding
          k <- k+1 # advance to the next flood event number
          dc <- dc+1 # advance the daily counter
          floods$wyr[k] <- d$wyr[j] # Enter the water year at the start of the flood
          floods$an_vol[k] <- d$an_vol[j] # Enter the total annual flow for the water year
          floods$start[k] <-  d$dt[j] # Enter the start date of the flood
          floods$start_wym[k] <- ifelse(month(d$dt[j])>=10,month(d$dt[j])-9,month(d$dt[j])+3) # Enter the wy month of the start of the flood event
          floods$start_d[k] <- d$wyrd[j] # Enter the water year day of the start of the flood event
          floods$cflw[k] <- d$cflw[j] # Enter the cumulative flow for the water year at the start of flood
          flooddays <- flooddays+1 # Add a flood day to counter
          if (d$flw[j]>floods$flw_pk[k]) {
            floods$pk_d[k] <- d$wyrd[j] # Assign the wy day of the peak flow
            floods$flw_pk[k] <- d$flw[j] # Add the peak flow
          }
          meanflow <- meanflow+d$flw[j] # Add to the running total of flow
          volume <- volume+d$flw[j]*3600*24 # Add to the running total of volume
          vol_v[flooddays] <- volume # Add to the vector of running total of volume
          r_rising <- min(r_rising,d$flw[j-1]-d$flw[j]) # Find the biggest difference in flow between days on rising limb
          r_falling <- max(r_falling,d$flw[j]-nxtflw) # Find the biggest difference in flow between days on falling limb
          notflood <- 0 # Switch to flood conditions
          # Fill in data for daily
          daily$wyr[dc] <- d$wyr[j] # Enter the water year at the start of the flood
          daily$wyrd[dc] <- d$wyrd[j] # Enter water year day
          daily$mo[dc] <- month(d$dt[j]) # Enter month
          daily$day[dc] <- day(d$dt[j]) # Enter day
          daily$yr[dc] <- year(d$dt[j]) # Enter year
          daily$event_no[dc] <- k # Enter the flood event id
          daily$fday[dc] <- flooddays # Enter the flood day
          daily$flw[dc] <- d$flw[j] # Enter the flow that day
          daily$dt[dc] <- d$dt[j] # Enter the date
          daily$an_vol[dc] <- d$an_vol[j] # Enter annual volume
          daily$hflw[dc] <- d$hflw[j] # Enter recent high flow
          daily$limb[dc] <- as.character(d$limb[j]) # Enter if on a rising or falling limb
        }
        else { # when it's during a flood
          flooddays <- flooddays+1 # Add a flood day to counter
          dc <- dc+1
          if (d$flw[j]>floods$flw_pk[k]) {
            floods$pk_d[k] <- d$wyrd[j] # Assign the wy day of the peak flow
            floods$flw_pk[k] <- d$flw[j] # Add the peak flow
          }
          meanflow <- meanflow+d$flw[j] # Add to the running total of flow
          volume <- volume+d$flw[j]*3600*24 # Add to the running total of volume
          vol_v[flooddays] <- volume # Add to the vector of running total of volume
          r_rising <- min(r_rising,d$flw[j-1]-d$flw[j]) # Find the biggest difference in flow between days on rising limb
          r_falling <- max(r_falling,d$flw[j]-nxtflw) # Find the biggest difference in flow between days on falling limb
          # Finding the peaks
          if ((d$flw[j]-nxtflw) > 0 & trigger_pk == 0) {
            pk_cnt <- pk_cnt+1
            trigger_pk <- 1
            trigger_vly <- 0
          }
          if ((d$flw[j-1]-d$flw[j]) == 0 & trigger_pk == 0 & (d$flw[j]-nxtflw) > 0) {
            pk_cnt <- pk_cnt+1
            trigger_pk <- 1
            trigger_vly <- 0
          }
          if ((d$flw[j]-nxtflw) < 0 & trigger_vly == 0) {
            trigger_vly <- 1
            trigger_pk <- 0
          }
          # Fill in data for daily
          daily$wyr[dc] <- d$wyr[j] # Enter the water year
          daily$wyrd[dc] <- d$wyrd[j] # Enter water year day
          daily$mo[dc] <- month(d$dt[j]) # Enter month
          daily$day[dc] <- day(d$dt[j]) # Enter day
          daily$yr[dc] <- year(d$dt[j]) # Enter year
          daily$event_no[dc] <- k # Enter the flood event id
          daily$fday[dc] <- flooddays # Enter the flood day
          daily$flw[dc] <- d$flw[j] # Enter the flow that day
          daily$dt[dc] <- d$dt[j] # Enter the date
          daily$an_vol[dc] <- d$an_vol[j] # Enter annual volume
          daily$hflw[dc] <- d$hflw[j] # Enter recent high flow
          daily$limb[dc] <- as.character(d$limb[j]) # Enter if on a rising or falling limb
        }
      }
    }

    floods <- floods[apply(floods[,-1], 1, function(x) !all(x==0)),] # Remove rows of 0s
    assign(paste("floods",round(Q[i]),sep=""),floods) # Create a data frame that stores the output
    f_data[[i]] <- floods # Store the info for that threshold value into the list
    f_filenames[i] <- paste0("floodid_events",round(Q[i])) # Create a vector of filenames
    write.csv(floods, file = paste0(outdir,"floods",round(Q[i]),".csv"),row.names = FALSE) # Write the files

    daily <- na.omit(daily) # daily[apply(daily[,-1], 1, function(x) !all(x==0)),] # Remove rows of 0s
    assign(paste("daily",round(Q[i]),sep=""),daily) # Create a data frame that stores the output
    daily_data[[i]] <- daily
    write.csv(daily,file=paste0(outdir,"floodid_daily",round(Q[i]),".csv"),row.names = FALSE) # Write the daily files

  }

  output<-list(f_data,f_filenames,daily_data)

  return(output)

}
