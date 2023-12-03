#' Fit function for positive OR negative sign
#'
#' @description Fits function for positive OR negative sign (signum_TF)
#' @return optimObject, to which fitting result of the best fit ('fitted'),
#' optimization measure value of this fit ('value').
#' @param optimObject optimObject, which is a list containing input data frame
#' with time resolved data ('data'),
#' the vector of initial guesses ('initialGuess.vec'), of
#' lower bounds ('lb.vec'),
#' of upper bounds ('ub.vec'), vector of fixed parameters ('fixed'),
#' if log10 is applied to bounds ('takeLog10'), the parameters having no
#' negative values in initialGuess.vec, lb.vec, and
#' ub.vec ('positive.par.names'),
#' modus ('modus'), and a list of values of fitted parameters ('fitted').
#' @param parStr String of variable based on which comparison is conducted (Default: "signum_TF")
#' @param parVal Value of variable defined in parString  (Default: 1)
#' @param nInitialGuesses Integer corresponding to number of initial guesses 
#' (Default: 50)
#' @export getBestFittingResult
#' @examples
#' data <- getExampleDf()
#' optimObject.orig <- initializeOptimObject(data,
#'                                          modus = 'RetardedTransientDynamics')
#' res.all.plus <- getBestFittingResult(
#'         optimObject.orig, parStr = "signum_TF", parVal = 1)

getBestFittingResult <- function(
  optimObject, parStr = "signum_TF", parVal = 1, nInitialGuesses = 50) {
  
  optimObject$fixed[[parStr]] <- parVal
  optimObject.woptimRes <- getMultiStartResults(
    optimObject, objFunct, nInitialGuesses = nInitialGuesses)
  
  res.pars <- optimObject.woptimRes$bestOptimResult$par
  value <- optimObject.woptimRes$bestOptimResult$value
  optimObject$fitted <- res.pars
  # optimObject$fitted <- c(res.pars, parVal)
  # names(optimObject$fitted) <- c(names(res.pars), parStr)
  optimObject$value <- value
  #optimObject$bestFit.plot <- optim.res$bestFit.plot
  
  optimObject$optimResults <- optimObject.woptimRes$optimResults
  optimObject$bestOptimResult <- optimObject.woptimRes$bestOptimResult
  optimObject
}
