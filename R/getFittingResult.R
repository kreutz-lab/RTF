#' Fit function for positive AND negative sign
#'
#' @description Fits function for positive AND negative sign (signum_TF)
#' @return List of two optimObjects for a positive and a negative signum_TF
#' ('plus1' and 'minus1'). Each including fitting result of the best
#' fit ('fitted'), optimization measure value of this fit ('value').
#' @param optimObject optimObject, which is a list containing input data frame
#' with time resolved data ('data'),
#' the vector of initial guesses ('initialGuess.vec'), of lower bounds
#' ('lb.vec'),
#' of upper bounds ('ub.vec'), vector of fixed parameters ('fixed'),
#' if log10 is applied to bounds ('takeLog10'), the parameters having no
#' negative values in initialGuess.vec, lb.vec, and ub.vec
#' ('positive.par.names'),
#' modus ('modus'), and a list of values of fitted parameters ('fitted')
#' @param parStr Parameter for which different parameter values
#' parVals are used for the fitting (Default: "signum_TF") 
#' @param parVals Vector of different values of parameter parStr used for
#' fitting (Default: c(-1, 1))
#' @param nInitialGuesses Integer corresponding to number of initial guesses 
#' (Default: 100)
#' @export getFittingResult
#' @examples
#' data <- getExampleDf()
#' optimObject.orig <- initializeOptimObject(data, modus = 'timeDependent')
#' res.all.plusMinus <- getFittingResult(
#'             optimObject.orig, 
#'             parStr = "signum_TF", parVals = c(-1, 1))

getFittingResult <- function(optimObject, parStr = "signum_TF", 
                             parVals = c(-1, 1), nInitialGuesses = 100) {
  for (pname in names(optimObject$initialGuess.vec)) {
    # for(pname in names(optimObject$fixed)){ # was replaced because signum_TF
    # is fixed but is ot listed in lb.vec, ub.vec, and initialGuess.vec
    if (!is.na(optimObject$fixed[pname]))
      optimObject$initialGuess.vec[pname] <-
        optimObject$lb.vec[pname] <-
        optimObject$ub.vec[pname] <-
        optimObject$fixed[pname]
  }
  # optimObject.orig <- optimObject

  # parStr <- "signum_TF"
  # parVals <- c(-1, 1)
  res.lst <- list()
  for (parVal in parVals) {
    optim.res <- NULL
    optim.res <- getBestFittingResult(
      optimObject, parStr = parStr, parVal = parVal,
      nInitialGuesses = nInitialGuesses)
    res.lst <- append(res.lst, list(optim.res))
  }

  names(res.lst) <- parVals
  res.lst
}
