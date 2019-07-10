#' Function to reorder raster files
#'
#' This function prepares the flows table that will be used in
#' hydrospatial analysis, taking as input the flows table from the
#' 'utils_flowstopredict' function.
#'
#' @param fls Vector of filenames with numbers to order after the last "_"
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @importFrom purrr map_chr
#' @export
#' @return Reordered filenames

utils_rasterreord <- function(fls) {

  rsnums <- tibble(fls) %>%
    mutate(rsnum = map_chr(fls, function(s) rev(strsplit(s,"_")[[1]])[1])) %>% # reorder
    mutate(rsnum = as.numeric(strsplit(rsnum,".grd")))
  rsnums <- rsnums[order(rsnums$rsnum),]

  return(rsnums$fls)

}
