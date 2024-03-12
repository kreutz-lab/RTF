#' Run RTF
#'
#' @description Run RTF
#' @return List of the final RTF model (finalModel) and the optimized parameters
#' (finalParams).
#' @param data Data frame containing columns named 't' (time) and
#' 'y' (quantitative value) for modus = 'timeDependent' ('TD'), and columns 
#' 't', 'y', and 'd' (dose) for modus = 'doseDependent' ('DD')
#' @param modus String indicating if modus 'timeDependent' ('TD') or
#' 'doseDependent' ('DD') should be used.
#' If no modus is provided default setting are
#' 'doseDependent' if column with name 'd' is present
#' and else 'timeDependent'.
#' @param optimFunction String indicating the optimization function which
#' should be used (Default: "logLikelihood")
#' @param nInitialGuesses Integer indicating number of initial guesses 
#' (in addition to the default initial guess) used both for a signum_TF of -1 
#' and 1 (Default: 100).
#' @param plotAllPointsWaterfall Boolean indicating if all points should be 
#' plotted in waterfall plot (Default: FALSE). 
#' If FALSE, all values up to the median of those values are plotted.
#' @param control List of control arguments passed to the function stats::optim
#' (Default: list(trace = 1, maxit = 1000, factr = 1e3))
#' @param resOld (Optional) Result of a previous runRTF run, with which results 
#' of current run will be combined
#' @export runRTF
#' @examples
#' modus <- "timeDependent"
#' data <- getExampleDf()
#' plotData(data)
#' res <- runRTF(data, modus = modus)

runRTF <- function(data,
                   modus = NULL,
                   optimFunction = "logLikelihood",
                   nInitialGuesses = 100,
                   plotAllPointsWaterfall = FALSE,
                   control = list(trace = 1,
                                  maxit = 1000,
                                  factr = 1e3
                                  ),
                   resOld = NULL) {
  if (is.null(modus)) {
    if (("d" %in% names(data))) {
      modus <- 'doseDependent'
    } else {
      modus <- 'timeDependent'
    }
  } else {
    if (modus == 'TD')
      modus <- 'timeDependent'
    if (modus == 'DD')
      modus <- 'doseDependent'
  }
  
  if (modus != "doseDependent" &
      !all(c("t", "y") %in% names(data))) {
    stop("Input data frame needs to contain the columns 't' and 'y'.")
  } else if (modus == "doseDependent" &
             !all(c("t", "y", "d") %in% names(data))) {
    stop("Input data frame needs to contain the columns 't', 'y', and 'd'.")
  }
  
  optimObject.orig <- initializeOptimObject(data,
                                            modus = modus,
                                            optimFunction = optimFunction,
                                            control = control)
  
  parsWithIdenticalBounds <- names(
    optimObject.orig$lb.vec[optimObject.orig$lb.vec == optimObject.orig$ub.vec])
  for (el in parsWithIdenticalBounds) {
    optimObject.orig$fixed[[el]] <- optimObject.orig$lb.vec[[el]]
  }
  
  res <- getInitialGuessResults(
    optimObject.orig, nInitialGuesses = nInitialGuesses)
  
  if (!is.null(resOld)) {
    # combine resOld[["finalModel"]][["optimResults"]] and 
    # res[["finalModel"]][["optimResults"]] to optimResults
    optimResults <- c(res$optimResults, 
                      resOld$finalModel$optimResults)
    
    optimResults <- sortListByValue(optimResults)
    res$optimResults <- optimResults
    res$bestOptimResult <- optimResults[[1]]
    res$value <- res$bestOptimResult$value
    res$fitted <- res$bestOptimResult$par
  }
  
  finalParams <- res$fitted
  
  print("The parameters of the best fit are:")
  print(paste(
    names(finalParams),
    signif(finalParams, 4),
    sep = ": ",
    collapse = ", "
  ))
  print("OptimValue:")
  print(res[["bestOptimResult"]][["value"]])
  
  return(
    list(finalModel = res, finalParams = finalParams)
  )
}
