#' Fit function for positive OR negative sign
#'
#' @description Fits function for positive OR negative sign (signum_TF)
#' @return optimObject, to which fitting result of the best fit ('fitted'),
#' optimization measure value of this fit ('value'), and, if plot=TRUE, the
#' plot to this fit ('bestFit.plot') has been added.
#' @param optimObject optimObject, which is a list containing input data frame
#' with time resolved data ('data'),
#' the vector of initial guesses ('initialGuess.vec'), of
#' lower bounds ('lb.vec'),
#' of upper bounds ('ub.vec'), vector of fixed parameters ('fixed'),
#' if log10 is applied to bounds ('takeLog10'), the parameters having no
#' negative values in initialGuess.vec, lb.vec, and
#' ub.vec ('positive.par.names'),
#' modus ('modus'), and a list of values of fitted parameters ('fitted').
#' @param parString String of variable based on which comparison is conducted (Default: "signum_TF")
#' @param parVal Value of variable defined in parString  (Default: 1)
#' @param plot Boolean value indicating if fitting results should be plotted
#' and saved as file
#' @param titlePrefix Prefix of file names of plots, if plot  was set
#' to TRUE
#' @export getBestFittingResult
#' @examples
#' data <- getExampleDf()
#' optimObject.orig <- initializeOptimObject(data,
#'                                          modus = 'RetardedTransientDynamics')
#' res.all.plus <- getBestFittingResult(
#'         optimObject.orig, parStr = "signum_TF", parVal = 1, plot = TRUE,
#'         titlePrefix = "fullModel_plus_")

getBestFittingResult <- function(
#    optimObject, signum_TF, plot, titlePrefix = "") {
  optimObject, parStr = "signum_TF", parVal = 1, plot, titlePrefix = "") {
  optimObject$fixed[[parStr]] <- parVal
  optim.res <- fittingHelper(
    optimObject, plot = plot, nInitialGuesses = 50, titlePrefix = titlePrefix)
  res.pars <- optim.res$res.pars
  value <- optim.res$value
  optimObject$fitted <- c(res.pars, parVal)
  names(optimObject$fitted) <- c(names(res.pars), parStr)
  optimObject$value <- value
  optimObject$bestFit.plot <- optim.res$bestFit.plot
  optimObject
}
