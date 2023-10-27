#' Generate plots
#'
#' @description Plot fitting results for optimObjects of modus 
#' "RetardedTransientDynamics" or "DoseDependentRetardedTransientDynamics"
#' @return ggplot object
#' @param optimObject optimObject containing elements "finalModel" and "modus"
#' @param fileNamePrefix File name prefix. If length>0 plots will be written to
#' file, otherwise they will be plotted directly.
#' @param plotAllFits Boolean indicating if all fits should be plotted. Only use
#' if fileNamePrefix is given. Only relevant for "RetardedTransientDynamics" modus.
#' @export plotRTF
#' @examples
#' data.doseResponse <- getDoseResponseExampleDf()
#' plotData(data.doseResponse)
#' res.doseResponse <- runRTF(data.doseResponse)

plotRTF <- function(optimObject, fileNamePrefix = "", plotAllFits = TRUE) {
  modus <- optimObject[["finalModel"]][["modus"]]
  if (modus == "RetardedTransientDynamics"){
    plotMultiStartPlots(optimObject = optimObject$finalModel, 
                        fileNamePrefix = fileNamePrefix, plotAllFits = plotAllFits)
  } else if (modus == "DoseDependentRetardedTransientDynamics") {
    plotDoseResponse(optimObject, fileNamePrefix) 
  }
}
