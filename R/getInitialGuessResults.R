#' Fit function for initial guesses
#'
#' @description Fits function for initial guesses
#' @return optimObject including fitting result of the best
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
#' @param nInitialGuesses Integer indicating number of initial guesses 
#' (in addition to the default initial guess) used both for a signum_TF of -1 
#' and 1 (Default: 100).
#' @export getInitialGuessResults
#' @examples
#' data <- getSimData()
#' optimObject.orig <- initializeOptimObject(data, modus = 'timeDependent')
#' res.all <- getInitialGuessResults( optimObject.orig)

getInitialGuessResults <- function(optimObject, nInitialGuesses = 100) {
  for (pname in names(optimObject$initialGuess.vec)) {
    # for(pname in names(optimObject$fixed)){ # was replaced because signum_TF
    # is fixed but is not listed in lb.vec, ub.vec, and initialGuess.vec
    if (!is.na(optimObject$fixed[pname]))
      optimObject$initialGuess.vec[pname] <-
        optimObject$lb.vec[pname] <-
        optimObject$ub.vec[pname] <-
        optimObject$fixed[pname]
  }

  initialGuess.vec.lst <- getInitialGuessVec(
    initialGuess.vec = optimObject$initialGuess.vec,
    lb.vec = optimObject$lb.vec,
    ub.vec = optimObject$ub.vec,
    takeLog10 = optimObject$takeLog10,
    nInitialGuesses = nInitialGuesses)
  
  initialGuessResults <-
    runOptimization(initialGuess.vec.lst, optimObject, objFunct)
  
  optimObject$optimResults <- initialGuessResults[["optimResults"]]
  
  optimObject$bestOptimResult <- initialGuessResults[["bestOptimResult"]]
  optimObject$fitted <- optimObject$bestOptimResult$par
  optimObject$value <- optimObject$bestOptimResult$value
  
  optimObject
}
