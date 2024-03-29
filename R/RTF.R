#' Run RTF
#'
#' @description Run RTF
#' @return List of the final RTF model ('finalModel') and the optimized 
#' parameters ('finalParams').
#' @param df Data frame containing columns named 't' (time) and
#' 'y' (quantitative value) for modus = 'singleDose' ('SD'), and columns 
#' 't', 'y', and 'd' (dose) for modus = 'doseDependent' ('DD'). 
#' Optionally, a column 'sigmaExp' can be provided with the standard error of 
#' the experimental data.
#' @param modus String indicating if modus 'singleDose' ('SD') or
#' 'doseDependent' ('DD') should be used.
#' If no modus is provided default setting are 'doseDependent' if column with 
#' name 'd' is present and else 'singleDose'.
#' @param optimFunction String indicating the optimization function which
#' should be used (Default: 'logLikelihood')
#' @param nInitialGuesses Integer indicating number of initial guesses 
#' (in addition to the default initial guess) used both for a signum_TF of -1 
#' and 1 (Default: 50).
#' @param plotAllPointsWaterfall Boolean indicating if all points should be 
#' plotted in waterfall plot (Default: FALSE). 
#' If FALSE, all values up to the median of those values are plotted.
#' @param control List of control arguments passed to the function stats::optim
#' (Default: list(trace = 1, maxit = 1000, factr = 1e7))
#' @param resOld (Optional) Result of a previous RTF run, with which results 
#' of current run will be combined
#' @export RTF
#' @examples
#' # Single-dose RTF
#' data <- getSimData()
#' res <- RTF(data, modus = "singleDose")
#' 
#' # Dose-dependent RTF
#' data <- getSimData(modus = "doseDependent")
#' res <- RTF(data, modus = "doseDependent")

RTF <- function(df,
                modus = NULL,
                optimFunction = "logLikelihood",
                nInitialGuesses = 50,
                plotAllPointsWaterfall = FALSE,
                control = list(trace = 1,
                               maxit = 1000,
                               factr = 1e7
                ),
                resOld = NULL) {
  if (is.null(modus)) {
    if (("d" %in% names(df))) {
      modus <- 'doseDependent'
    } else {
      modus <- 'singleDose'
    }
  } else {
    if (modus == 'SD')
      modus <- 'singleDose'
    if (modus == 'DD')
      modus <- 'doseDependent'
  }
  
  if (modus != "doseDependent" &
      !all(c("t", "y") %in% names(df))) {
    stop("Input data frame needs to contain the columns 't' and 'y'.")
  } else if (modus == "doseDependent" &
             !all(c("t", "y", "d") %in% names(df))) {
    stop("Input data frame needs to contain the columns 't', 'y', and 'd'.")
  }
  
  optimObject.orig <- initializeOptimObject(df,
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
  print("Likelihood value:")
  print(res[["bestOptimResult"]][["value"]])
  
  return(
    list(finalModel = res, finalParams = finalParams)
  )
}
