#' Get best fit for initial guesses
#'
#' @description Get best fit for predefined number of initial guesses
#' @return List with the parameters of the best fit ('res.pars'),
#' the optimization measure value of the best fit ('value'),
#' and the ggplot object of the plot of the best fit ('bestFit.plot')
#' @param optimObject optimObject, which is a list containing input data frame with time resolved data ('data'),
#' the vector of initial guesses ('initialGuess.vec'), of lower bounds ('lb.vec'),
#' of upper bounds ('ub.vec'), vector of fixed parameters ('fixed'),
#' if log10 is applied to bounds ('takeLog10'), the parameters having no
#' negative values in initialGuess.vec, lb.vec, and ub.vec ('positive.par.names'),
#' modus ('modus'), and a list of values of fitted parameters ('fitted').
#' optimObject$fixed[["signum_TF"]] has to be set to 1 or -1
#' @param plot Boolean value indicating if fitting results should be plotted and saved as file.
#' Plots include for each fit plot of the components of the transient function
#' and the position of the fit in the waterfall plot based on the value of the
#' optimization measure the fit reached. For the best fit the components of
#'  the transient function are plotted as well as the parameter distribution
#'  over all fits and a water fall plot of all fits.
#' @param nInitialGuesses Number of initial guesses
#' @param titlePrefixPrefix Prefix of file names of plots, if plot  was set to TRUE
#' @export fittingHelper
#' @examples
#' data <- getExampleDf()
#' data <- scaleTimeCol(data)
#' optimObject.orig <- initializeOptimObject(data,
#'                                          modus = 'RetardedTransientDynamics')
#' signum_TF <- 1
#' optimObject.orig$fixed[["signum_TF"]] <- signum_TF
#' optim.res <- fittingHelper(optimObject.orig, plot = TRUE,
#'                             titlePrefix = "fullModel_plus_")

fittingHelper <- function(optimObject, plot = TRUE, nInitialGuesses = 50, titlePrefix = "") {
  res.lst.wFinal <- getInitialGuessResults(
    optimObject, objFunct, nInitialGuesses, plot = plot)
  final <- res.lst.wFinal$final

  bestFit.plot <- NA
  if (plot) {
    library(patchwork)

    waterfallPlotData <- final$gg.waterfall$data

    res.lst <- res.lst.wFinal$res.lst
    res.lst.gg <- lapply(res.lst, function(x) x[["gg"]])

    pdf(file = paste0(titlePrefix, "allFits.pdf"), width = 12, height = 10)
    for (i in seq(length(res.lst.gg))) {
      #print(i)
      gg <- res.lst.gg[[i]]
      #library(patchwork)
      gg.wWaterfall <-  gg + plotWaterfallPlot(waterfallPlotData, i)
      print(gg.wWaterfall)
      #gg
    }
    dev.off()

    bestFit.plot <- final$gg + final$gg.waterfall +
      final$gg.paramDistr + patchwork::plot_layout(ncol = 2)
    ggplot2::ggsave(filename = paste0(titlePrefix, "bestFit.pdf"),
           bestFit.plot, width = 12, height = 13)
  }

  list(res.pars = final$optimRes$par,
       value = final$optimRes$value,
       bestFit.plot = bestFit.plot)
}
