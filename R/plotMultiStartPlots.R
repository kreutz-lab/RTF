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
#' @param fileNamePrefix File name prefix. If length>0 plots will be written to
#' file, otherwise they will be plotted directly.
#' @param plotAllFits Boolean indicating if all fits should be plotted. Only use
#' if fileNamePrefix is given.
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
#' pl <- plotMultiStartPlots(optim.res, fileNamePrefix = "ExamplePlot")

plotMultiStartPlots <- function(optimObject, fileNamePrefix = "", 
                                plotAllFits = TRUE) {
  
  saveToFile <- nchar(fileNamePrefix) > 0
  
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
  
  waterfallPlotData <- gg.waterfall$data
  
  if (plotAllFits) {
    if(saveToFile) {
      gg.lst <- list()
      for (i in seq(length(optimResTmpLstParsAll))){
        pars <- optimResTmpLstParsAll[[i]]
        # print(pars)
        value <- optimResTmpLstValuesAll[[i]]
        
        title <- paste0("OptimValue: ", round(value, 2),
                        "; ", paramsToNotBeFitted, ": ", optimObject$fixed[[paramsToNotBeFitted]], ", ",
                        paste(names(pars), round(pars, 4), sep = ": ", collapse = ", "))
        
        gg <- plotRTFComponents(pars = pars,
                                data = optimObject$data,
                                signum_TF = optimObject$fixed[[paramsToNotBeFitted]], title = title)
        
        gg.lst <- append(gg.lst, list(gg = gg))
        
      }
      
      grDevices::pdf(file = paste0(fileNamePrefix, "_allFits.pdf"), width = 12, height = 10)
      for (i in seq(length(gg.lst))) {
        gg <- gg.lst[[i]]
        gg.wWaterfall <-  gg + plotWaterfallPlot(waterfallPlotData, i)
        print(gg.wWaterfall)
      }
      grDevices::dev.off()
    } else {
      message("Please enter a file name to save all fits to file.")
    }
  }

  bestFit.plot <- gg.final + gg.waterfall +
    gg.paramDistr + plot_layout(ncol = 2)
  
  if (saveToFile){
    ggplot2::ggsave(filename = paste0(fileNamePrefix, "_bestFit.pdf"),
                    bestFit.plot, width = 12, height = 13)
  } else {
    bestFit.plot
  }
}
