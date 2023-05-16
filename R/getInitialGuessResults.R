#' @description Get fits for defined number of initial guesses and add optional
#' plots (incl. for best fit ('final') waterfall plot and for all parameters
#' histograms of the parameter values across all fits)
#' @return List of with optimization results and plots to each fit ('res.lst')
#' and optimization result and plot to best fit ('final'), both with optional plots
#' @param optimObject optimObject, which is a list containing input data frame with time resolved data ('data'),
#' the vector of initial guesses ('initialGuess.vec'), of lower bounds ('lb.vec'),
#' of upper bounds ('ub.vec'), vector of fixed parameters ('fixed'),
#' if log10 is applied to bounds ('takeLog10'), the parameters having no
#' negative values in initialGuess.vec, lb.vec, and ub.vec ('positive.par.names'),
#' modus ('modus'), and a list of values of fitted parameters ('fitted').
#' optimObject$fixed[["signum_TF"]] has to be set to 1 or -1
#' @param objFunct Name of the objective function
#' @param nInitialGuesses Integer corresponding to number of initial guesses
#' @param plot Boolean value indicating if fitting results should be plotted and saved as file.
#' Plots include for each fit plot of the components of the transient function
#' and the position of the fit in the waterfall plot based on the value of the
#' optimization measure the fit reached. For the best fit the components of
#'  the transient function are plotted as well as the parameter distribution
#'  over all fits and a water fall plot of all fits.
#' @export getInitialGuessResults
#' @examples
#' data <- getExampleDf()
#' data <- scaleTimeCol(data)
#' optimObject.orig <- initializeOptimObject(data, modus = 'RetardedTransientDynamics')
#' signum_TF <- 1
#' optimObject.orig$fixed[["signum_TF"]] <- signum_TF
#' nInitialGuesses <- 50
#' optim.res <- getInitialGuessResults(optimObject.orig, objFunct, nInitialGuesses, plot = TRUE)

getInitialGuessResults <- function(optimObject, objFunct, nInitialGuesses, plot = TRUE) {
  initialGuess.vec.lst <- getInitialGuessVec(
    initialGuess.vec = optimObject$initialGuess.vec,
    lb.vec = optimObject$lb.vec,
    ub.vec = optimObject$ub.vec,
    nInitialGuesses = nInitialGuesses)

  initialGuessResults <- runOptimization(initialGuess.vec.lst, optimObject, objFunct)

  res.lst <- initialGuessResults[["res.lst"]]
  bestOptimRes <- initialGuessResults[["bestOptimRes"]]

  gg.final <- gg.waterfall <- gg.paramDistr <- NA

  if (plot){
    res.lst.optimRes <- lapply(res.lst, function(x) {
      tmp <- unlist(x[grep("optimRes", names(x))], recursive = FALSE)
      names(tmp) <- sub("optimRes.", "", names(tmp))
      tmp
    })

    optimResTmpLstParsAll <- lapply(res.lst.optimRes, function(x) {
      tmp <- unlist(x[grep("par",names(x))])
      names(tmp) <- sub("par.", "", names(tmp))
      tmp
    })

    optimResTmpLstValuesAll <- unlist(
      lapply(res.lst.optimRes, function(x) unlist(x[grep("value",names(x))])))

    gg.waterfall <- plotWaterfallPlot(optimResTmpLstValuesAll)

    optimResTmpLstParsAll.df <- data.frame(do.call(rbind, optimResTmpLstParsAll))
    optimResTmpLstParsAll.df.long <- reshape2::melt(optimResTmpLstParsAll.df)
    gg.paramDistr <- plotParameterDistribution(optimResTmpLstParsAll.df.long)

    title <- paste0("OptimValue: ", round(bestOptimRes$value, 2),
                    "; signum_TF: ", optimObject$fixed[["signum_TF"]], ", ",
                    paste(names(bestOptimRes$par),
                          round(bestOptimRes$par, 4), sep = ": ", collapse = ", "))
    gg.final <- plotModelComponents(
      pars = bestOptimRes$par,
      data = optimObject$data,
      signum_TF = optimObject$fixed[["signum_TF"]], title = title
    )
  }

  final <- list(optimRes = bestOptimRes,
                gg = gg.final,
                gg.waterfall = gg.waterfall,
                gg.paramDistr = gg.paramDistr)

  res.lst.wFinal <- list(res.lst = res.lst, final = final)
  res.lst.wFinal
}
