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
#' modus <- "doseDependent" # "timeDependent"
#' data <- getExampleDf(modus = modus)
#' optimObject.orig <- initializeOptimObject(data,
#'                         modus = modus)
#' optimObject.orig$fixed[["signum_TF"]] <- 1
#' optimObject.orig$fixed[["M_gamma"]] <- 0.5 # optimObject.orig$fixed[["gamma"]] <- 2
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
#' paramsToBeFitted <- names(initialGuess.vec.lst[[1]])
#'
#' pars.tmp <- c()
#' # Remove each fixedParam from vec, optimObject$lb.vec, and optimObject$ub.vec
#' for (el in paramsToBeFitted) {
#'   if (!is.na(optimObject.tmp$fixed[[el]])) {
#'     nam <- names(pars.tmp)
#'     pars.tmp <- c(pars.tmp, optimObject.tmp$fixed[[el]])
#'     names(pars.tmp) <- c(nam, el)
#'     optimObject.tmp$lb.vec <- optimObject.tmp$lb.vec[
#'       -which(names(optimObject.tmp$lb.vec) == el)]
#'     optimObject.tmp$ub.vec <- optimObject.tmp$ub.vec[
#'       -which(names(optimObject.tmp$ub.vec) == el)]
#'     
#'     # remove from each sublist in initialGuess.vec.lst
#'     initialGuess.vec.lst <- lapply(initialGuess.vec.lst,
#'                                    function(x) x[-which(names(x) == el)])
#'   }
#' }
#' 
#' takeLog10 <- optimObject.tmp$takeLog10 
#' 
#' lower <- applyLog10ForTakeLog10(optimObject.tmp$lb.vec, takeLog10)
#' upper <- applyLog10ForTakeLog10(optimObject.tmp$ub.vec, takeLog10)
#' 
#' # Take logarithm of fixed parameters for which takeLog10 = TRUE
#' fixed <- optimObject.tmp$fixed
#' intersectFixedAndTakeLog10 <- intersect(names(fixed[!is.na(fixed)]), 
#'                                         names(which(takeLog10)))
#' fixedAndTakeLog10 <- rep(NA, length(takeLog10))
#' names(fixedAndTakeLog10) <- names(takeLog10)
#' fixedAndTakeLog10[intersectFixedAndTakeLog10] <- TRUE
#' optimObject.tmp$fixed <- applyLog10ForTakeLog10(fixed, fixedAndTakeLog10)
#' 
#' vec <- initialGuess.vec.lst[[1]]
#' 
#' vec <- applyLog10ForTakeLog10(vec, takeLog10)
#' parscale <- rep.int(1,length(vec))
#' names(parscale) <- names(vec)
#' rangeY <- max(optimObject.tmp$data$y, na.rm = TRUE) - 
#'   min(optimObject.tmp$data$y, na.rm = TRUE)
#' 
#' for (parameter in c("A", "B", "b", "sigma", "M_A", "M_B")) {
#'   if (parameter %in% names(vec)) parscale[parameter] <- rangeY
#' }
#' 
#' if (optimObject.tmp$modus != "doseDependent") {
#'   # ndeps
#'   ndeps <- vec
#'   for (paramName in names(ndeps)) {
#'     if (takeLog10[[paramName]]) {
#'     ndeps[[paramName]] <- min(1e-3, lower[[paramName]])
#'     } else {
#'       ndeps[[paramName]] <- min(
#'         1e-3, (upper[[paramName]] - lower[[paramName]]) * 1e-5)
#'     }
#'   }
#'   
#'   optimObject.tmp$control <- append(optimObject.tmp$control,
#'                                     list(parscale = parscale,
#'                                          ndeps = ndeps))
#' } else {
#'   optimObject.tmp$control <- append(optimObject.tmp$control,
#'                                     list(parscale = parscale))
#' }
#'
#' optimResTmp <- stats::optim(par = vec,
#'                             fn = objFunct,
#'                             method = "L-BFGS-B",
#'                             lower = optimObject.tmp$lb.vec,
#'                             upper = optimObject.tmp$ub.vec,
#'                             data = optimObject.tmp$data,
#'                             optimObject = optimObject.tmp,
#'                             calcGradient = TRUE,
#'                             control = optimObject.tmp$control)

objFunct <- function(par, data, optimObject, calcGradient = FALSE) {
  retval <- NULL
  
  parOrder <- names(par)
  
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
  
  if (calcGradient) {
    dpar_dpar <- applyLog10ForTakeLog10(par, optimObject[["takeLog10"]], 
                                        reverse = TRUE, calcGradient = TRUE)
  }
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
  
  dretval_dres <- matrix(nrow = length(d), ncol = 1)
  for (id in 1:length(ds)) { # loop over all doses
    ind <- which(d == ds[id]) # indices where dose matches
    
    hillF <- hillGradient <- NULL
    if (optimObject$modus == "doseDependent") {
        hillF <- getHillResults(d = ds[id], params = par)
        dhillF_dpar <- getHillResults(d = ds[id],
                                      params = par,
                                      calcGradient = TRUE) # length(rtfPara) x length(par), hier length(par) so was wie 15
        # hillF
        # rtfPar <- .. hillF... # TODO: rtfPar
        # drtfPar_dpar <- drtfPar_dhillF %*% dhillF_dpar # TODO: drtfPar_dhillF
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
      dretval_dsigma <- 0
      dretval_dres[ind,] <- sum((2 * res[ind]) / sigma[ind]^2)
    } else {
      
      tmp.res <- exp(res[ind]^2 / (2 * sigma^2))
      tmp.res[is.infinite(tmp.res)] <- 10^20
      dretval_dsigma <- 
        sum(2 * sigma * tmp.res * 
              (exp(-res[ind]^2 / (2 * sigma^2)) / (sigma^2 * (2 * pi)^(1 / 2)) - 
                 (res[ind]^2 * exp(-res[ind]^2 / (2 * sigma^2))) / 
                 (sigma^4 * (2 * pi)^(1 / 2))) * (2 * pi)^(1 / 2)) 
                # TODO
                # NaN = Inf * 0, because 2 * sigma * exp(res[ind]^2 / (2 * sigma^2))
                # can be Inf 
      dretval_dres[ind,] <- sum((2 * res[ind]) / sigma^2) # TODO Correct with ind?
    }
    
    
  }
  
  # stats::dnorm: (exp((-0.5 * (res / sigma)^2))) / (sigma * (2 * pi)^(0.5))
  # retval <- sum(-2 * log(stats::dnorm(res, mean = 0, sd = sigma))) 

  # >> diff(-2 * log((exp((-0.5 * (res / sigma)^2))) / (sigma * (2 * pi)^(0.5))), sigma)
  #  2*sigma*exp(res^2/(2*sigma^2))*(exp(-res^2/(2*sigma^2))/(sigma^2*(2*pi)^(1/2)) - (res^2*exp(-res^2/(2*sigma^2)))/(sigma^4*(2*pi)^(1/2)))*(2*pi)^(1/2)
  #>> diff(-2 * log((exp((-0.5 * (res / sigma)^2))) / (sigma * (2 * pi)^(0.5))), res)
  #  (2*res)/sigma^2
  
  retval <- sum(-2 * log(
    (exp((-0.5 * (res / sigma)^2))) / (sigma * (2 * pi)^(0.5))
    )) 
  
  ################################
  # TODO: Are the following lines correct?
  dretval_dpar <- t(dretval_dres) %*% dres_dpar 
  colnames(dretval_dpar) <- names(par)
  
  dsigma_dpar <- t(array(0, dim = length(parOrder)))
  colnames(dsigma_dpar) <- parOrder
  dsigma_dpar[parOrder == "sigma"] <- 1
  
  v3 <- c(dretval_dpar[1,], (dretval_dsigma %*% dsigma_dpar)[1,])
  dretval_dpar <- tapply(v3, names(v3), sum) # Sum elements of same name in  named vectors
  dretval_dpar <- dretval_dpar[parOrder]
  # dretval_dpar <- dretval_dpar + dretval_dsigma %*% dsigma_dpar # TODO dsigma_dpar = 1 x length(par) mit 1 da wo names(par)=="sigma"
  
  colNamesdpar_dpar <- colnames(dpar_dpar) 
  dpar_dpar2 <- cbind(dpar_dpar, rep(0, nrow(dpar_dpar)))
  dpar_dpar2 <- rbind(dpar_dpar2, c(rep(0, ncol(dpar_dpar2) - 1), 1))
  colnames(dpar_dpar2) <- c(colNamesdpar_dpar, "sigma")
  
  
  rownames(dpar_dpar2) <- colnames(dpar_dpar2)
  dpar_dpar2 <- dpar_dpar2[parOrder, parOrder]
  
  ##############################################
  
  dretval_dpar <- dretval_dpar %*% dpar_dpar2 # account log-trsf, sigma by initiation is not logarithmized by initiation
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
