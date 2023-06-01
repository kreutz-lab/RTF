#' Get list of initial guesses
#'
#' @description Get list of default initial guess for each model parameter and
#' a defined number of further initial guesses lying in between the lower and
#' upper bound of each model parameter
#' @return List of default initial guess for each model parameter
#' a defined number of further initial guesses lying in between the lower and
#' upper bound of each model parameter
#' @param initialGuess.vec Vector of the default initial guesses for each
#' model parameter
#' @param lb.vec Vector of the lower bounds for each model parameter
#' @param ub.vec Vector of the upper bounds for each model parameter
#' @param nInitialGuesses Integer corresponding to number of initial guesses
#' @export getInitialGuessVec
#' @examples
#' data <- getExampleDf()
#' data <- scaleTimeCol(data)
#' optimObject.orig <- initializeOptimObject(data,
#'                                          modus = 'RetardedTransientDynamics')
#' initialGuess.vec <- optimObject.orig[["initialGuess.vec"]]
#' lb.vec <- optimObject.orig[["lb.vec"]]
#' ub.vec <- optimObject.orig[["ub.vec"]]
#' nInitialGuesses <- 50
#' initialGuess.vec.lst <- getInitialGuessVec(
#'                           initialGuess.vec, lb.vec, ub.vec, nInitialGuesses)

getInitialGuessVec <- function(initialGuess.vec,
                               lb.vec,
                               ub.vec,
                               nInitialGuesses) {
  # For 50 different random initial guesses between bounds
  initialGuess.vec.lst <- list()
  initialGuess.vec.lst[[length(initialGuess.vec.lst) + 1]] <- initialGuess.vec

  for (i in 1:nInitialGuesses){
    randomPortions <- stats::runif(length(initialGuess.vec))
    randomInitialGuess.vec <- (randomPortions*lb.vec) + ((1-randomPortions)*ub.vec)
    initialGuess.vec.lst[[length(initialGuess.vec.lst) + 1]] <- randomInitialGuess.vec
  }

  initialGuess.vec.lst
}
