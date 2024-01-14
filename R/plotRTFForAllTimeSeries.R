#' Generate plots based on the list of RTF parameters to multiple time series
#'
#' @description Generates plots using plotRTF based on the RTF paramaters 
#' derived for each of multiple time series. 
#' @return pdf file, where each page corresponds to one time series.
#' @param res.lst List with the RTF result for each time series
#' @param fileString String that should be added to file name
#' @param height Integer indicating page height
#' @param width Integer indicating page width
#' @export plotRTFForAllTimeSeries
#' @examples
#' data(strasenTimeSeries)
#' df.multipleTimeSeries <- strasenTimeSeries[, 1:3]
#' res.lst <- runRTFOnMultipleTimeSeries(df.multipleTimeSeries)
#' plotRTFForAllTimeSeries(res.lst)

plotRTFForAllTimeSeries <- function(res.lst, fileString = "", 
                                 height = 12, width = 10) {
  grDevices::pdf(paste0("modelPlots_", fileString,".pdf"), height = height, width = width)
  for (i in seq(length(res.lst))) {
    el <- res.lst[[i]]
    print(plotRTF(el, plotTitle = names(res.lst)[i]))
  }
  grDevices::dev.off()
}