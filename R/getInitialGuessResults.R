#' Fit function for initial guesses
#'
#' @description Fits function for initial guesses
#' @return optimObject including the fitting result of the best fit ('fitted'),
#' list of the stats::optim results for all initial guesses ('optimResults'),
#' the stats::optim result of the best fit ('bestOptimResult'),
#' and the log-likelihood value of the best fit ('value').
#' @param optimObject optimObject, which is a list containing input data frame
#' with time-resolved data ('data'), the vector of initial guesses
#' ('initialGuess.vec'), of lower bounds ('lb.vec'), of upper bounds ('ub.vec'),
#' vector of fixed parameters ('fixed'), if log10 is applied to bounds
#' ('takeLog10'), the parameters having no negative values in initialGuess.vec,
#' lb.vec, and ub.vec ('positive.par.names'), modus ('modus'),
#' log-likelihood function for the parameter optimization ('optimFunction'), if
#' the sign of the sustained and transient RTF part should be equal
#' ('sameSign'), list of control parameters passed to stats::optim ('control'),
#' and a list of values of fitted parameters ('fitted', can be empty).
#' @param nInitialGuesses Integer indicating number of initial guesses
#' (in addition to the default initial guess) used for all four combinations
#' of signSus and signTrans, which both can be -1 or 1 (Default: 50).
#' @export getInitialGuessResults
#' @examples
#' data <- getSimData()
#' optimObject.orig <- initializeOptimObject(data, modus = "singleDose")
#' res.all <- getInitialGuessResults(optimObject.orig)
getInitialGuessResults <- function(optimObject, nInitialGuesses = 50) {
    for (pname in names(optimObject$initialGuess.vec)) {
        if (!is.na(optimObject$fixed[pname])) {
            optimObject$initialGuess.vec[pname] <-
                optimObject$lb.vec[pname] <-
                optimObject$ub.vec[pname] <-
                optimObject$fixed[pname]
        }
    }

    initialGuess.vec.lst <- getInitialGuessVecLst(
        initialGuess.vec = optimObject$initialGuess.vec,
        lb.vec = optimObject$lb.vec,
        ub.vec = optimObject$ub.vec,
        takeLog10 = optimObject$takeLog10,
        nInitialGuesses = nInitialGuesses
    )

    initialGuessResults <-
        runOptimization(initialGuess.vec.lst, optimObject, objFunct)

    optimObject$optimResults <- initialGuessResults[["optimResults"]]

    optimObject$bestOptimResult <- initialGuessResults[["bestOptimResult"]]
    optimObject$fitted <- optimObject$bestOptimResult$par
    optimObject$value <- optimObject$bestOptimResult$value

    optimObject
}
