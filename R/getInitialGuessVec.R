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
#' @param takeLog10 Vector of booleans indicating if log10 is/should be taken
#' of the model parameters.
#' @param nInitialGuesses Integer corresponding to number of initial guesses
#'  (Default: 100)
#' @export getInitialGuessVec
#' @examples
#' data <- getExampleDf()
#' optimObject.orig <- initializeOptimObject(data,
#'                                          modus = 'timeDependent')
#' initialGuess.vec <- optimObject.orig[["initialGuess.vec"]]
#' lb.vec <- optimObject.orig[["lb.vec"]]
#' ub.vec <- optimObject.orig[["ub.vec"]]
#' takeLog10 <- optimObject.orig[["takeLog10"]]
#' nInitialGuesses <- 100
#' initialGuess.vec.lst <- getInitialGuessVec(
#'                           initialGuess.vec, lb.vec, ub.vec, takeLog10,
#'                           nInitialGuesses)

getInitialGuessVec <- function(initialGuess.vec,
                               lb.vec,
                               ub.vec,
                               takeLog10,
                               nInitialGuesses = 100) {
  # For 50 different random initial guesses between bounds
  initialGuess.vec.lst <- list()
  initialGuess.vec.lst[[length(initialGuess.vec.lst) + 1]] <- initialGuess.vec
  
  lb.vec <- applyLog10ForTakeLog10(lb.vec, takeLog10, reverse = FALSE) 
  ub.vec <- applyLog10ForTakeLog10(ub.vec, takeLog10, reverse = FALSE)   
  
  for (i in 1:nInitialGuesses) {
    randomPortions <- stats::runif(length(initialGuess.vec))
    randomInitialGuess.vec <- 
      (randomPortions * lb.vec) + ((1 - randomPortions) * ub.vec)
    randomInitialGuess.vec <- applyLog10ForTakeLog10(randomInitialGuess.vec, 
                                                     takeLog10, reverse = TRUE)   
    initialGuess.vec.lst[[length(initialGuess.vec.lst) + 1]] <- 
      randomInitialGuess.vec
  }

  initialGuess.vec.lst
}
