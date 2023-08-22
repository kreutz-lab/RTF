#' Plot multi start plots
#'
#' @description Plot fitting results.
#' Plots include for each fit plot of the components of the transient function
#' and the position of the fit in the waterfall plot based on the value of the
#' optimization measure the fit reached. For the best fit the components of
#' the transient function are plotted as well as the parameter distribution
#' over all fits and a water fall plot of all fits.
#' @return ggplot object
#' @param optimObject optimObject, which contains optimResults and bestOptimResult
#' @param titlePrefix File prefix
#' @param plotAllFits Boolean indicating if all fits should be plotted
#' @export plotMultiStartPlots
#' @import patchwork 
#' @examples
#' data <- getExampleDf()
#' optimObject.orig <- initializeOptimObject(
#'                 data, modus = 'RetardedTransientDynamics')
#' signum_TF <- 1
#' optimObject.orig$fixed[["signum_TF"]] <- signum_TF
#' nInitialGuesses <- 50
#' optim.res <- getMultiStartResults(
#'                 optimObject.orig, objFunct, nInitialGuesses)
#' pl <- plotMultiStartPlots(optim.res, titlePrefix = "ExamplePlot")

plotMultiStartPlots <- function(optimObject, titlePrefix = "", plotAllFits = TRUE) {
  
  optimResults <- optimObject$optimResults
  bestOptimResult <- optimObject$bestOptimResult
  
  if (is.null(optimResults) | is.null(bestOptimResult)){
    stop("optimResults and bestOptimResult needs to be available in the optimObject.")
  }

  gg.final <- gg.waterfall <- gg.paramDistr <- NA
  optimResults.optimRes <- lapply(optimResults, function(x) {
    tmp <- unlist(x[grep("optimRes", names(x))], recursive = FALSE)
    names(tmp) <- sub("optimRes.", "", names(tmp))
    tmp
  })
  
  optimResTmpLstParsAll <- lapply(optimResults.optimRes, function(x) {
    tmp <- unlist(x[grep("par",names(x))])
    names(tmp) <- sub("par.", "", names(tmp))
    tmp
  })
  
  optimResTmpLstValuesAll <- unlist(
    lapply(optimResults.optimRes, function(x) unlist(x[grep("value",names(x))])))
  
  gg.waterfall <- plotWaterfallPlot(optimResTmpLstValuesAll)
  
  optimResTmpLstParsAll.df <- data.frame(
    do.call(rbind, optimResTmpLstParsAll))
  optimResTmpLstParsAll.df.long <- reshape2::melt(optimResTmpLstParsAll.df)
  gg.paramDistr <- plotParameterDistribution(optimResTmpLstParsAll.df.long)
  
  # paramsToNotBeFitted <- setdiff(names(optimObject$fixed), paramsToBeFitted)
  
  paramsToNotBeFitted <- names(which(!is.na(optimObject$fixed)))
  title <- paste0("OptimValue: ", round(bestOptimResult$value, 2),
                  "; ", paramsToNotBeFitted, ": ", optimObject$fixed[[paramsToNotBeFitted]], ", ",
                  paste(names(bestOptimResult$par),
                        round(bestOptimResult$par, 4),
                        sep = ": ", collapse = ", "))
  gg.final <- plotRTFComponents(
    pars = bestOptimResult$par,
    data = optimObject$data,
    signum_TF = optimObject$fixed[[paramsToNotBeFitted]],
    title = title
  )
  
  optimResults.gg <- lapply(optimResults, function(x) x[["gg"]])
  waterfallPlotData <- gg.waterfall$data
  
  if (plotAllFits) {
    pdf(file = paste0(titlePrefix, "_allFits.pdf"), width = 12, height = 10)
    for (i in seq(length(optimResults.gg))) {
      #print(i)
      gg <- optimResults.gg[[i]]
      library(patchwork)
      gg.wWaterfall <-  gg + plotWaterfallPlot(waterfallPlotData, i)
      print(gg.wWaterfall)
      #gg
    }
    dev.off()
  }

  
  bestFit.plot <- gg.final + gg.waterfall +
    gg.paramDistr + plot_layout(ncol = 2)
  
  ggplot2::ggsave(filename = paste0(titlePrefix, "_bestFit.pdf"),
         bestFit.plot, width = 12, height = 13)
}
