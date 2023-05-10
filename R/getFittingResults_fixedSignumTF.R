#' @description Fits function for positive or negative sign (signum_TF)
#' @return List of fitting results for positive ('plus1') or negative sign ('minus1') of function (signum_TF)
#' @param optimObject optimObject, which is a list containing input data frame with time resolved data ('data'),
#' the vector of initial guesses ('initialGuess.vec'), of lower bounds ('lb.vec'),
#' of upper bounds ('ub.vec'), vector of fixed parameters ('fixed'),
#' if log10 is applied to bounds ('takeLog10'), the parameters having no
#' negative values in initialGuess.vec, lb.vec, and ub.vec ('positive.par.names'),
#' modus ('modus'), and a list of values of fitted parameters ('fitted').
#' @param signum_TF 1 if sign of function is positive, -1 if it is negative
#' @param plot Boolean value indicating if fitting results should be plotted and saved as file
#' @param titlePrefixPrefix Prefix of file names of plots, if plot  was set to TRUE
#' @export getFittingResults_fixedSignumTF
#' @examples
#' data <- getExampleDf()
#' data <- scaleTimeCol(data)
#' optimObject.orig <- initializeOptimObject(data, modus = 'RetardedTransientDynamics')
#' res.all.plus <- getFittingResults_fixedSignumTF(optimObject.orig, signum_TF = 1, plot = TRUE, titlePrefixPrefix = "fullModel_plus_")

getFittingResults_fixedSignumTF <- function(
    optimObject, signum_TF, plot, titlePrefix = "") {
  optimObject$fixed[["signum_TF"]] <- signum_TF
  optim.res <- fittingHelper(optimObject, plot = plot, titlePrefix = titlePrefix)
  res.pars <- optim.res$res.pars
  value <- optim.res$value
  optimObject$fitted <- c(res.pars, signum_TF = signum_TF)
  optimObject$value <- value
  optimObject$bestFit.plot <- optim.res$bestFit.plot
  optimObject
}
