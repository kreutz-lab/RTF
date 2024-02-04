#' Generate plots based on the list of RTF parameters to multiple time series
#'
#' @description Generates plots using plotRTF based on the RTF paramaters 
#' derived for each of multiple time series. 
#' @return pdf file, where each page corresponds to one time series.
#' @param res.lst List with the RTF result for each time series
#' @param fileString String that should be added to file name
#' @param height Integer indicating page height
#' @param width Integer indicating page width
#' @param plotFitsToSingleFile Boolean indicating if plots should be returned as a
#' single file.
#' @param plotFitOnly Plot fit only without additional information as provided 
#' using function plotRTF(). 
#' @export plotRTFForAllTimeSeries
#' @examples
#' data(strasenTimeSeries)
#' df.multipleTimeSeries <- strasenTimeSeries[, 1:3]
#' res.lst <- runRTFOnMultipleTimeSeries(df.multipleTimeSeries)
#' plotRTFForAllTimeSeries(res.lst)

plotRTFForAllTimeSeries <- function(res.lst, fileString = "", 
                                 height = 12, width = 10,
                                 plotFitsToSingleFile = TRUE,
                                 plotFitOnly = FALSE) {
  
  if (plotFitsToSingleFile)
    grDevices::pdf(paste0("modelPlots_", fileString,".pdf"), height = height, 
                   width = width)
  for (i in seq(length(res.lst))) {
    el <- res.lst[[i]]
    title <- names(res.lst)[i]
    
    if (!plotFitsToSingleFile)
      grDevices::pdf(paste0("modelPlot_", gsub("/", "_", title),".pdf"), 
                     height = height, 
                     width = width)
    
    if (plotFitOnly) {
      optimObject <- el$finalModel
      bestOptimResult <- optimObject$bestOptimResult
      par <- bestOptimResult$par
      value <- bestOptimResult$value
      data <- optimObject$data
      
      
      plotTitle <- paste0(title, "; OptimValue: ", round(value, 2),
                      "; ", 
                      paste(names(par), 
                            round(par, 4), 
                            sep = ": ", collapse = ", "))
      plotTitle <- paste(strwrap(plotTitle, width = 70), collapse = "\n")
      
      print(plotFit(par = par,
              y = data$y, 
              t = data$t, 
              modus = "RetardedTransientDynamics",
              withData = TRUE,
              title = plotTitle))
    } else {
      print(plotRTF(el, plotTitle = title))
    }
    
    if (!plotFitsToSingleFile)
      grDevices::dev.off()
  }
  
  if (plotFitsToSingleFile)
    grDevices::dev.off()
}
