#' Generate plots
#'
#' @description Plot fitting results for optimObjects of modus
#' "RetardedTransientDynamics" or "DoseDependentRetardedTransientDynamics"
#' @return ggplot object
#' @param optimObject optimObject containing elements "finalModel" and "modus"
#' @param fileNamePrefix File name prefix. If length>0 plots will be written to
#' file, otherwise they will be plotted directly. Default is empty string.
#' @param plotTitle Title of the plot (Default is no title).
#' @param plotAllFits Boolean indicating if all fits should be plotted. Only use
#' if fileNamePrefix is given. Only relevant for "RetardedTransientDynamics"
#' modus. (Default: TRUE)
#' using function plotRTF().
#' @param plotAllPointsWaterfall Boolean indicating if all points should be 
#' plotted in waterfall plot (Default: FALSE). 
#' If FALSE, all values up to the median of those values are plotted.
#' @export plotRTF
#' @examples
#' data.doseResponse <- getExampleDf(
#'                             modus = "DoseDependentRetardedTransientDynamics")
#' plotData(data.doseResponse)
#' res.doseResponse <- runRTF(data.doseResponse, modelReduction = FALSE)
#' plotRTF(res.doseResponse, plotAllFits = FALSE)

plotRTF <-
  function(optimObject,
           fileNamePrefix = "",
           plotTitle = "",
           plotAllFits = TRUE,
           plotAllPointsWaterfall = FALSE) {
    optimObject <- optimObject$finalModel
    
    saveToFile <- nchar(fileNamePrefix) > 0
    
    modus <- optimObject$modus
    data <- optimObject$data
    optimResults <- optimObject$optimResults
    bestOptimResult <- optimObject$bestOptimResult
    
    if (is.null(optimResults) | is.null(bestOptimResult)) {
      stop("optimResults and bestOptimResult needs to be available in the optimObject.")
    }
    
    par <- bestOptimResult$par
    
    numCol <- 1
    if (modus == "RetardedTransientDynamics") {
      height <- 12
    } else if (modus == "DoseDependentRetardedTransientDynamics") {
      height <- 17
    }
    
    ############################################################################
    waterfallPlot <- parDistributionPlot <- NA
    
    optimResults.optimRes <- lapply(optimResults, function(x) {
      tmp <- unlist(x[grep("optimRes", names(x))], recursive = FALSE)
      names(tmp) <- sub("optimRes.", "", names(tmp))
      tmp
    })
    
    optimResTmpLstParsAll <-
      lapply(optimResults.optimRes, function(x) {
        tmp <- unlist(x[grep("par", names(x))])
        names(tmp) <- sub("par.", "", names(tmp))
        tmp
      })
    
    ############################################################################
    
    optimResTmpLstValuesAll <- unlist(lapply(optimResults.optimRes, function(x)
      unlist(x[grep("value",
                    names(x))])))
    
    waterfallPlot <- plotWaterfallPlot(optimResTmpLstValuesAll, 
                                       plotAllPoints = plotAllPointsWaterfall)
    
    ############################################################################
    
    waterfallPlotData <- waterfallPlot$data
    
    if (plotAllFits) {
      if (saveToFile) {
        gg.lst <- list()
        for (i in seq(length(optimResTmpLstParsAll))) {
          pars <- optimResTmpLstParsAll[[i]]
          value <- optimResTmpLstValuesAll[[i]]
          
          title <- paste0(
            "OptimValue: ",
            signif(value, 2),
            "; ",
            paste(
              names(pars),
              signif(pars, 4),
              sep = ": ",
              collapse = ", "
            )
          )
          title <- paste(strwrap(title, width = 80), collapse = "\n")
          if (modus == "RetardedTransientDynamics") {
            gg <- plotFit(
              par = pars,
              y = data$y,
              t = data$t,
              withData = TRUE
            ) +
              ggplot2::ggtitle(title)
            
          } else if (modus == "DoseDependentRetardedTransientDynamics") {
            gg <- plotFit(
              pars,
              withData = TRUE,
              modus = optimObject$modus,
              y = data$y,
              t = data$t,
              d = data$d,
              title = title
            )
          }
          
          gg.lst <- append(gg.lst, list(gg = gg))
          
        }
        
        grDevices::pdf(
          file = paste0(fileNamePrefix, "_", modus, "_allFits.pdf"),
          width = 8,
          height = 10
        )
        for (i in seq(length(gg.lst))) {
          gg <- gg.lst[[i]]
          waterfall.tmp <- plotWaterfallPlot(
            waterfallPlotData, i, plotAllPoints = plotAllPointsWaterfall)
          waterfallPlot2 <- patchwork::wrap_plots(
            gg, waterfall.tmp, ncol = numCol)
          print(waterfallPlot2)
        }
        grDevices::dev.off()
      } # else {
        # message("Please enter a file name to save all fits to file.")
      # }
    }
    
    ############################################################################
    
    optimResTmpLstParsAll.df <- data.frame(
      do.call(rbind, optimResTmpLstParsAll))
    optimResTmpLstParsAll.df.long <-
      reshape2::melt(optimResTmpLstParsAll.df)
    parDistributionPlot <-
      plotParameterDistribution(optimResTmpLstParsAll.df.long)
    
    ############################################################################
    
    title <- paste0(
      "OptimValue: ",
      signif(bestOptimResult$value, 2),
      "; ",
      paste(
        names(par),
        signif(par, 4),
        sep = ": ",
        collapse = ", "
      )
    )
    title <- paste(strwrap(title, width = 100), collapse = "\n")
    
    if (modus == "RetardedTransientDynamics") {
      RTFComponentsPlot <- plotFit(
        par = par,
        y = data$y,
        t = data$t,
        withData = TRUE
      ) # + ggplot2::ggtitle(title)
      
      bestFit.plot <-
        patchwork::wrap_plots(RTFComponentsPlot,
                              waterfallPlot,
                              parDistributionPlot,
                              ncol = numCol)
      
    } else if (modus == "DoseDependentRetardedTransientDynamics") {
      bestFitWDataPlot <- plotFit(
        par,
        withData = TRUE,
        modus = modus,
        y = data$y,
        t = data$t,
        d = data$d
      )
      
      ##################################################
      d <- seq(0, max(data[["d"]]), length.out = 1000)
      
      df <- getHillResults(d = d, params = par)
      
      df.long <- reshape2::melt(df, id.vars = c("d"))
      doseParamPlot <- ggplot2::ggplot(df.long, ggplot2::aes(
        x = d, y = value, color = variable)) +
        ggplot2::geom_line() +
        ggplot2::xlab("Dose") +
        ggplot2::ylab("Value") +
        # ggplot2::ggtitle(title) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_blank())
      
      bestFit.plot <- patchwork::wrap_plots(doseParamPlot,
                                            bestFitWDataPlot,
                                            waterfallPlot,
                                            parDistributionPlot,
                                            ncol = numCol)
    }
    
    ############################################################################
    
    # if (nchar(plotTitle) > 0 ) {
    bestFit.plot <-
      bestFit.plot + patchwork::plot_annotation(title = plotTitle,
                                                subtitle = title)
    #}
    
    if (saveToFile) {
      ggplot2::ggsave(
        filename = paste0(fileNamePrefix, "_", modus,
                          "_bestFit.pdf"),
        bestFit.plot,
        width = 8,
        height = height
      )
      bestFit.plot
    } else {
      bestFit.plot
    }
  }