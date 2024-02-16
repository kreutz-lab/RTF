#' Chi square optimization function
#'
#' @description Chi square optimization function
#' @return List of best set of parameters, optimization value, etc.
#' See stats::optim for more details.
#' @param par Initial values for the parameters to be optimized over.
#' @param data Data frame containing columns named 't' (time), 'y' (quantitative
#' value) and optionally 'sdExp' (standard deviation of the experimental data)
#' @param optimObject optimObject
#' @param calcGradient Boolean indicating if gradient should be calculated
#' (Default: FALSE)
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
#'                             takeLog10 = optimObject.orig$takeLog10,
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

objFunct <- function(par, data, optimObject, calcGradient = FALSE) {
  retval <- NULL
  
  fixed <- optimObject[["fixed"]]
  signum_TF <- fixed[["signum_TF"]]
  if (!is.na(fixed[["sigma"]])) {
    sigma <- fixed[["sigma"]]
  } else {
    sigma <- par[["sigma"]]
  }
  
  allParamNames <- setdiff(names(fixed), c("signum_TF", "sigma"))
  rtfParamNames <- c("alpha", "gamma", "A", "B", "b", "tau")
  
  fixed <- fixed[allParamNames]
  par <- par[setdiff(names(par), "sigma")]
  
  data <- data[stats::complete.cases(data), ] 
  
  if ("d" %in% colnames(data)) {
    d <- data$d
  } else {
    d <- rep(1, nrow(data))
  }
  
  # Fixed parameters will overwrite the values in par
  for (v in 1:length(fixed)) {
    if (!is.na(fixed[[v]])) {
      par[names(fixed)[v]] <- fixed[[v]]
    }
  }
  par <- par[allParamNames] # Bring par in correct order
  
  
  # lowerReg <- applyLog10ForTakeLog10(optimObject$lb.vec, optimObject$takeLog10)
  # upperReg <- applyLog10ForTakeLog10(optimObject$ub.vec, optimObject$takeLog10)
  # lowerReg <- lowerReg[names(par)]
  # upperReg <- upperReg[names(par)]
  # meanReg <- rowMeans(cbind(lowerReg, upperReg), na.rm = TRUE)
  # regularizationTerm <- sum(((par - meanReg)^2) /
  #                             (((upperReg - lowerReg)^2) * 100))
  
  dpar_dpar <- applyLog10ForTakeLog10(par, optimObject[["takeLog10"]], 
                                      reverse = TRUE, calcGradient = TRUE) # TODO
  par <- applyLog10ForTakeLog10(par, optimObject[["takeLog10"]], 
                                reverse = TRUE, calcGradient = FALSE)
  
  
  dparAfterFix_dpar <- matrix(0, nrow = length(par), ncol = length(par))
  rownames(dparAfterFix_dpar) <- colnames(dparAfterFix_dpar) <- names(par)
  diag(dparAfterFix_dpar) <- 1
  overlap <- intersect(names(fixed[!is.na(fixed)]), allParamNames) 
  dparAfterFix_dpar[names(par) %in% overlap, names(par) %in% overlap] <- 0
  
  
  ds <- unique(d)
  res <- array(NA, dim = length(d))
  dres_dpar <- matrix(nrow = length(d), ncol = length(rtfParamNames))
  for (id in 1:length(ds)) { # loop over all doses
    ind <- which(d == ds[id]) # indices where dose matches
    
    hillF <- hillGradient <- NULL
    if (optimObject$modus == "doseDependent") {
        hillF <- getHillResults(d = ds[id], params = par)
        dhillF_dpar <- getHillResults(d = ds[id],
                                          params = par, calcGradient = TRUE) # length(rtfPara) x length(par), hier length(par) so was wie 15
        hillF
        # rtfPar <- .. hillF...
        # drtfPar_dpar <- drtfPar_dhillF %*% dhillF_dpar
        for (name in names(hillF)) {
          par[names(hillF)[name]] <- hillF[[name]]
        }
    } else {
      rtfPar <- par
      drtfPar_dpar <- matrix(0, nrow = length(rtfPar), ncol = length(rtfPar))
      diag(drtfPar_dpar) <- 1 # length(rtfPara) x length(par), length(par) so was wie 7 
      
    }
    
    yRtf <- getTransientFunctionResult(
      rtfPar = rtfPar,
      t = data$t[ind],
      signum_TF = signum_TF, 
      scale = TRUE, 
      calcGradient = FALSE)
    
    dyRtf_drtfPar <- getTransientFunctionResult(
      rtfPar = rtfPar,
      t = data$t[ind],
      signum_TF = signum_TF,
      scale = TRUE, 
      calcGradient = TRUE)
    
    # set derivates of fixed parameters to zero
    dyRtf_dpar <- dyRtf_drtfPar %*% drtfPar_dpar %*% dparAfterFix_dpar # length(data$y) x length(par)
    
    res[ind] <- data$y[ind] - yRtf  # entweder mit ind an die richtige Stelle schreiben oder mit yRtf mit rbind so zusammenbauen, dass es zu data$y passt (gleiche dosen und Zeiten in gleicher Zeile)
    dres_dpar[ind,] <- -dyRtf_dpar 
    
    if (("sdExp" %in% colnames(data))) {
      if (id == 1)
        sigma <- array(NA, dim = length(d))
      sigma[ind] <- data$sdExp[ind]
    } 
  }
  retval <- sum(-2 * log10(stats::dnorm(res, mean = 0, sd = sigma))) 
  dretval_dpar <- 
  
  # retval <- retval + regularizationTerm 
  
  if (retval > 10^20) {
    print(par)
    retval <- 10^20
    # warning(paste0("objective function is infinite."))
  } else if (retval < -10^20) {
    retval <- -10^20
  }
  
  return(retval)
}
