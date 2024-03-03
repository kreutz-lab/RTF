#' Run optimization
#'
#' @description Run optimization using stats::optim with method = "L-BFGS-B"
#' @return List of with optimization results('optimResults')
#' and best optimization result ('bestOptimResult')
#' @param initialGuess.vec.lst List of default initial guess for each model parameter
#' a defined number of further initial guesses lying in between the lower and
#' upper bound of each model parameter
#' @param optimObject optimObject, which is a list containing input data frame 
#' with time resolved data ('data'),
#' the vector of initial guesses ('initialGuess.vec'), of lower bounds ('lb.vec'),
#' of upper bounds ('ub.vec'), vector of fixed parameters ('fixed'),
#' if log10 is applied to bounds ('takeLog10'), the parameters having no
#' negative values in initialGuess.vec, lb.vec, and ub.vec ('positive.par.names'),
#' modus ('modus'), and a list of values of fitted parameters ('fitted').
#' optimObject$fixed[["signum_TF"]] has to be set to 1 or -1
#' @param objFunct Name of the objective function
#' @export runOptimization
#' @examples
#' data <- getExampleDf()
#' optimObject.orig <- initializeOptimObject(data,
#'                                          modus = 'timeDependent')
#' optimObject.orig$fixed[["signum_TF"]] <- 1
#' nInitialGuesses <- 100
#' initialGuess.vec.lst <- getInitialGuessVec(
#'                             initialGuess.vec =
#'                                           optimObject.orig$initialGuess.vec,
#'                             lb.vec = optimObject.orig$lb.vec,
#'                             ub.vec = optimObject.orig$ub.vec,
#'                             takeLog10 = optimObject.orig$takeLog10,
#'                             nInitialGuesses = nInitialGuesses
#' )
#' res <- runOptimization(initialGuess.vec.lst, optimObject.orig, objFunct)

runOptimization <- function(initialGuess.vec.lst, optimObject, objFunct) {
  currentBestResValue <- NULL
  res.lst <- list()
  optimObject.tmp <- optimObject
  
  ##############################################################################
  # Bring y data to range 0-10 and scale parameters accordingly, 
  # as optimization works better on higher values
  yDependentPars <- c("A", "B", "b", "sigma", "M_A", "M_B")
  scaleFactor <- 10/max(optimObject.tmp$data$y, na.rm = TRUE)
  optimObject.tmp$data$y <- optimObject.tmp$data$y * scaleFactor
  
  optimObject.tmp$lb.vec[names(optimObject.tmp$lb.vec) %in% yDependentPars] <-
    optimObject.tmp$lb.vec[names(optimObject.tmp$lb.vec) 
                           %in% yDependentPars] * scaleFactor
  
  optimObject.tmp$ub.vec[names(optimObject.tmp$ub.vec) %in% yDependentPars] <-
    optimObject.tmp$ub.vec[names(optimObject.tmp$ub.vec) 
                           %in% yDependentPars] * scaleFactor
  
  optimObject.tmp$fixed[names(optimObject.tmp$fixed) %in% yDependentPars] <-
    optimObject.tmp$fixed[names(optimObject.tmp$fixed) 
                          %in% yDependentPars] * scaleFactor
  
  for (i in seq(length(initialGuess.vec.lst))) {
    initialGuess.vec.lst[[i]][
      names(initialGuess.vec.lst[[i]]) %in% yDependentPars] <- 
      initialGuess.vec.lst[[i]][
        names(initialGuess.vec.lst[[i]]) %in% yDependentPars] * scaleFactor
  }
  ##############################################################################

  paramsToBeFitted <- names(initialGuess.vec.lst[[1]])
  pars.tmp <- c()
  # Remove each fixedParam from vec, optimObject$lb.vec, and optimObject$ub.vec
  for (el in paramsToBeFitted) {
#       c("alpha", "gamma", "A", "B", "b", "tau", "sigma")) {
    if (!is.na(optimObject.tmp$fixed[[el]])) {
      nam <- names(pars.tmp)
      pars.tmp <- c(pars.tmp, optimObject.tmp$fixed[[el]])
      names(pars.tmp) <- c(nam, el)
      optimObject.tmp$lb.vec <- optimObject.tmp$lb.vec[
        -which(names(optimObject.tmp$lb.vec) == el)]
      optimObject.tmp$ub.vec <- optimObject.tmp$ub.vec[
        -which(names(optimObject.tmp$ub.vec) == el)]

      # remove from each sublist in initialGuess.vec.lst
      initialGuess.vec.lst <- lapply(initialGuess.vec.lst,
                                     function(x) x[-which(names(x) == el)])
    }
  }

  takeLog10 <- optimObject.tmp$takeLog10 
  
  lower <- applyLog10ForTakeLog10(optimObject.tmp$lb.vec, takeLog10)
  upper <- applyLog10ForTakeLog10(optimObject.tmp$ub.vec, takeLog10)
  
  # Take logarithm of fixed parameters for which takeLog10 = TRUE
  fixed <- optimObject.tmp$fixed
  intersectFixedAndTakeLog10 <- intersect(names(fixed[!is.na(fixed)]), 
                                          names(which(takeLog10)))
  fixedAndTakeLog10 <- rep(NA, length(takeLog10))
  names(fixedAndTakeLog10) <- names(takeLog10)
  fixedAndTakeLog10[intersectFixedAndTakeLog10] <- TRUE
  optimObject.tmp$fixed <- applyLog10ForTakeLog10(fixed, fixedAndTakeLog10)
  
  for (vec in initialGuess.vec.lst) {
    print(vec)
    vec <- applyLog10ForTakeLog10(vec, takeLog10)

    optimResTmp <- optimx::optimr(par = vec,
                                  fn = objFunct,
                                  gr = objFunctGradient,
                                  method = "L-BFGS-B",
                                  lower = lower,
                                  upper = upper,
                                  data = optimObject.tmp$data,
                                  optimObject = optimObject.tmp,
                                  calcGradient = FALSE,
                                  control = optimObject.tmp$control)
    
    optimResTmp$par <- applyLog10ForTakeLog10(c(optimResTmp$par), 
                                              takeLog10, reverse = TRUE)
    
    vecOrder <- names(optimObject$fixed)
    parsFinal <- c(fixed[!is.na(fixed)], optimResTmp$par)[vecOrder]
    parsFinal[names(parsFinal) %in% yDependentPars] <-  
      parsFinal[names(parsFinal) %in% yDependentPars] / scaleFactor
    optimResTmp$par <- parsFinal
    
    value <- c(optimResTmp$value)

    res.lst <- append(res.lst, list(list(optimRes = optimResTmp)))

    if (is.null(currentBestResValue)) {
      currentBestResValue <- value
      optimRes <- optimResTmp
    }

    if (value < currentBestResValue) {
      currentBestResValue <- value
      optimRes <- optimResTmp
    }
  }

  res.lst <- sortListByValue(res.lst)

  list(optimResults = res.lst, bestOptimResult = optimRes)
}
