#' Function to summarize area-day from daily results
#'
#' This function takes the daily hsa results table from hydrospatial analysis
#' and summarizes annual on a given field, returning a list of an annual
#' 'timeseries' and a 'summary' of that time series
#'
#' @param d Data frame in format used for 'hsa' functions
#' @param sumby Character field name to summarize on
#' @importFrom dplyr group_by summarize_ filter rename_
#' @importFrom magrittr %>%
#' @importFrom lazyeval interp
#' @export
#' @return List of annual 'timeseries' and 'summary' of that time series

utils_areaday <- function(d, sumby){
  # For annual timeseries: Group by water year and sum the area across each year
    wy_s <- as.data.frame(d %>%
                            group_by(wyr) %>%
                            summarize_(sum_a=interp(~sum(v), v=as.name(sumby))))
    if ("ft" %in% colnames(d)) {
      fts <- sort(unique(d$ft))
      for (i in (1:length(fts))) {
        new <- paste0("sum_a_ft",fts[i])
        wy_s <- merge(wy_s, as.data.frame(d %>%
                                            filter(ft==fts[i]) %>%
                                            group_by(wyr) %>%
                                            summarize_(sum=interp(~sum(v), v=as.name(sumby)))),by="wyr",all="TRUE")
        wy_s <- rename_(wy_s, .dots=setNames("sum",new))
      }
    }

    ADay <- vector("list",2)
    names(ADay) <- c("timeseries","summary")
    ADay[[1]] <- wy_s
    ADay[[2]] <- data.frame(scl=NA, mean=NA, sd=NA, cv=NA)
    ADay[[2]]$scl <- "wyr"
    ADay[[2]]$mean <- mean(wy_s$sum_a)
    ADay[[2]]$sd <- sd(wy_s$sum_a)
    ADay[[2]]$cv <- cv(wy_s$sum_a)

    # For overall summary: Group by the event number, find the maximum area for each event, summarize by flood type for the mean, sd, and cv
    if ("ft" %in% colnames(d)) {
      d_g <- as.data.frame(d %>%
                               group_by(eno, wyr, ft) %>%
                               summarize_(sum_a=interp(~sum(v), v=as.name(sumby))) %>%
                               group_by(ft) %>%
                               summarize(mean=mean(sum_a), sd=sd(sum_a), cv=cv(sum_a)))
      d_g$ft <- paste0("ft",d_g$ft)
      names(d_g) <- c("scl", "mean", "sd", "cv")
      # Combine data frame
      ADay[[2]] <- rbind(ADay[[2]],d_g)
    }

  return(ADay)

}
