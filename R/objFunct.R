#' Chi square optimization function
#'
#' @description Chi square optimization function
#' @return List of best set of parameters, optimization value, etc.
#' See stats::optim for more details.
#' @param par Initial values for the parameters to be optimized over.
#' @param data Data frame containing columns named 't' (time), 'y' (quantitative
#' value) and optionally 'sdExp' (standard deviation of the experimental data)
#' @param optimObject optimObject
#' @export objFunct
#' @examples
#' data <- getExampleDf()
#' optimObject.orig <- initializeOptimObject(data,
#'                         modus = 'RetardedTransientDynamics')
#' optimObject.orig$fixed[["signum_TF"]] <- 1
#' optimObject.orig$fixed[["gamma"]] <- 2
#' nInitialGuesses <- 100
#' initialGuess.vec.lst <- getInitialGuessVec(
#'                             initialGuess.vec =
#'                                     optimObject.orig$initialGuess.vec,
#'                             lb.vec = optimObject.orig$lb.vec,
#'                             ub.vec = optimObject.orig$ub.vec,
#'                             nInitialGuesses = nInitialGuesses
#'  )
#' optimObject.tmp <- optimObject.orig
#'
#' # Remove each fixedParam from vec, optimObject$lb.vec, and optimObject$ub.vec
#' for (el in
#'       c("alpha", "gamma", "A", "B", "b", "tau", "sigma")){
#'   if (!is.na(optimObject.tmp$fixed[[el]])){
#'     optimObject.tmp$lb.vec <-
#'          optimObject.tmp$lb.vec[-which(names(optimObject.tmp$lb.vec) == el)]
#'     optimObject.tmp$ub.vec <-
#'          optimObject.tmp$ub.vec[-which(names(optimObject.tmp$ub.vec) == el)]
#'
#'     # remove from each sublist in initialGuess.vec.lst
#'     initialGuess.vec.lst <- lapply(initialGuess.vec.lst,
#'                                        function(x) x[-which(names(x) == el)])
#'   }
#' }
#'
#' vec <- initialGuess.vec.lst[[1]]
#'
#' optimResTmp <- stats::optim(par = vec,
#'                             fn = objFunct,
#'                             method = "L-BFGS-B",
#'                             lower = optimObject.tmp$lb.vec,
#'                             upper = optimObject.tmp$ub.vec,
#'                             data = optimObject.tmp$data,
#'                             optimObject = optimObject.tmp,
#'                             control = optimObject.tmp$control)

objFunct <- function(par, data, optimObject) {
  retval <- NULL
  
  if (!is.na(optimObject$fixed[["sigma"]])) {
    sigma <- optimObject$fixed[["sigma"]]
  } else {
    sigma <- par[["sigma"]]
  }
  
  data <- data[stats::complete.cases(data), ] 
  
  if ("d" %in% colnames(data)) {
    d <- data$d
  } else {
    d <- NULL
  }
  
  lowerReg <- applyLog10ForTakeLog10(optimObject$lb.vec, optimObject$takeLog10)
  upperReg <- applyLog10ForTakeLog10(optimObject$ub.vec, optimObject$takeLog10)
  lowerReg <- lowerReg[names(par)]
  upperReg <- upperReg[names(par)]
  meanReg <- rowMeans(cbind(lowerReg, upperReg), na.rm = TRUE)
  regularizationTerm <- sum(((par - meanReg)^2) /
                              (((upperReg - lowerReg)^2) * 100))
  
  # par[names(par) %in% names(which(optimObject[["takeLog10"]]))] <-
  #   10^par[names(par) %in% names(which(optimObject[["takeLog10"]]))]
  
  par <- applyLog10ForTakeLog10(par, optimObject[["takeLog10"]], reverse = TRUE)
  
  if (optimObject$optimFunction == "chiSquare") {
  
    res <- data$y - getTransientFunctionResult(par = par[names(par) != "sigma"],
                                               t = data$t,
                                               d = d,
                                               # fixed = fixed,
                                               fixed = optimObject$fixed,
                                               modus = optimObject$modus,
                                               scale = TRUE)
  
    if (("sdExp" %in% colnames(data))) {
      sigma <- data$sdExp
    } 

    retval <- sum(-2 * log(stats::dnorm(res, mean = 0, sd = sigma))) 
    
    retval <- retval + regularizationTerm 
    
    if (retval > 10^20) {
      print(par)
      retval <- 10^20
      # warning(paste0("objective function is infinite."))
    } else if (retval < -10^20) {
      retval <- -10^20
    }
  }
  return(retval)
}
