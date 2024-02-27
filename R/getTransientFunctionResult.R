#' Calculate RTF results for multiple time points
#'
#' @description Apply RTF with defined parameters for defined time points
#' @return Vector of predicted quantitative values resulting from applying RTF
#' with the specified parameters to time points given in t.
#' @param rtfPar Named vector of the parameter values used for RTF.
#' @param t Vector of time points
#' @param signum_TF Value of signum_TF (-1 or 1, Default: 1)
#' @param scale Boolean, indicates if time dependent parameters t, tau,
#' alpha, and gamma should be scaled
#' @param calcGradient Boolean indicating if gradient should be calculated
#' (Default: FALSE)
#' @export getTransientFunctionResult
#' @examples
#' rtfPar <- c(alpha = 1.00, gamma = 1.00, A = 1.05,
#'          B = 3.05, b = -0.28, tau = -1, signum_TF )
#' t <- c(0, 0.71, 1.42, 2.14, 2.85, 3.57= 1, 4.28, 5, 5.71, 6.42,
#'              7.14, 7.85, 8.57, 9.28, 10)
#' y <- getTransientFunctionResult(rtfPar = rtfPar, t = t, signum_TF = 1)
#' plot(t, y)

getTransientFunctionResult <- function(rtfPar = c(),
                                       t = NULL,
                                       signum_TF = 1,
                                       scale = TRUE, 
                                       calcGradient = FALSE) {
  
  # rtfParamNames <- c("alpha", "gamma", "A", "B", "b", "tau")
  # rtfPar <- rtfPar[rtfParamNames]
  rtfParamNames <- names(rtfPar)
  
  if (length(setdiff(c("alpha", "gamma", "A", "B", "tau", "b"), 
                     rtfParamNames)) > 0) {
    stop("Parameters alpha, gamma, A, B, tau, b need to be provided for getTransientFunctionResult().")
  }
  
  for (v in 1:length(rtfPar)) assign(names(rtfPar)[v], rtfPar[[v]])

  if (scale) {
    scaleRes <- scaleTimeParameter(timeParam = t, maxVal = 10 / max(t))
    t_prime <- scaleRes$timeParam
    maxVal <- scaleRes$maxVal
  } else {
    t_prime <- t
  }
  
  # scaling everything with time as phys. unit
  if (scale) {
    if (calcGradient) {
      dparScaled_dpar <- matrix(0, nrow = length(rtfPar), ncol = length(rtfPar))
      diag(dparScaled_dpar) <- 1
      
      dtau_dtau <- scaleTimeParameter(
        timeParam = c(tau = tau), 
        maxVal = maxVal, 
        gradientNames = "tau")$timeParam
      
      dalpha_dalpha <- scaleTimeParameter(
        timeParam = c(alpha = alpha), 
        maxVal = 1 / maxVal, 
        gradientNames = "alpha")$timeParam
      
      dgamma_dgamma <- scaleTimeParameter(
        timeParam = c(gamma = gamma), 
        maxVal = 1 / maxVal, 
        gradientNames = "gamma")$timeParam
      
      rownames(dparScaled_dpar) <- rtfParamNames
      colnames(dparScaled_dpar) <- rtfParamNames
      
      dparScaled_dpar["tau","tau"]   <- dtau_dtau
      dparScaled_dpar["alpha","alpha"] <- dalpha_dalpha
      dparScaled_dpar["gamma","gamma"] <- dgamma_dgamma
    }

    tau <- scaleTimeParameter(timeParam = c(tau = tau), 
                              maxVal = maxVal)$timeParam
    alpha <- scaleTimeParameter(
      timeParam = c(alpha = alpha), maxVal = 1 / maxVal)$timeParam
    gamma <- scaleTimeParameter(
      timeParam = c(gamma = gamma), maxVal = 1 / maxVal)$timeParam
  } else {
    if (calcGradient) {
      dparScaled_dpar <- matrix(0, nrow = length(rtfPar), ncol = length(rtfPar))
      diag(dparScaled_dpar) <- 1
    }
  }
  
  nonLinTransformation <- log10(10^t_prime + 10^tau) - log10(1 + 10^tau)
  
  if (calcGradient) {
    dnonLinTrans_dparScaledRtf <- matrix(0, 
                                   nrow = length(nonLinTransformation),
                                   ncol = length(rtfParamNames))
    
    colnames(dnonLinTrans_dparScaledRtf) <- rtfParamNames
    dnonLinTrans_dparScaledRtf[, "tau"] <- 
      10^tau / (10^t_prime + 10^tau) - 10^tau / (10^tau + 1)
  }

  transientFunctionRes <- 
    signum_TF * A * (1 - exp(-alpha * nonLinTransformation)) + 
    signum_TF * B * (1 - exp(-alpha * nonLinTransformation)) * 
    exp(-gamma * nonLinTransformation) + b
  
  if (calcGradient) {
    dtransFunRes_dnonLinTrans <- matrix(0, 
                                        nrow = length(transientFunctionRes), 
                                        ncol = length(nonLinTransformation))
    diag(dtransFunRes_dnonLinTrans) <- A * alpha * signum_TF * 
      exp(-alpha * nonLinTransformation) + B * gamma * signum_TF * 
      exp(-gamma * nonLinTransformation) * 
      (exp(-alpha * nonLinTransformation) - 1) + 
      B * alpha * signum_TF * exp(-alpha * nonLinTransformation) * 
      exp(-gamma * nonLinTransformation)
    
    dtransFunRes_dparScaledRtf <- matrix(0, 
                                   nrow = length(transientFunctionRes), 
                                   ncol = length(rtfParamNames))
    colnames(dtransFunRes_dparScaledRtf) <- rtfParamNames
    
    dtransFunRes_dparScaledRtf[,"alpha"] <- 
      A * signum_TF * nonLinTransformation * exp(-alpha * nonLinTransformation) + 
      B * signum_TF * nonLinTransformation * exp(-alpha * nonLinTransformation) * 
      exp(-gamma * nonLinTransformation)
    
    dtransFunRes_dparScaledRtf[,"gamma"] <- 
      B * signum_TF * nonLinTransformation * exp(-gamma * nonLinTransformation) * 
      (exp(-alpha * nonLinTransformation) - 1)
    dtransFunRes_dparScaledRtf[,"b"] <- 1
    
    dtransFunRes_dparScaledRtf[,"A"] <- 
      -signum_TF * (exp(-alpha * nonLinTransformation) - 1)
    
    dtransFunRes_dparScaledRtf[,"B"] <- 
      -signum_TF * exp(-gamma * nonLinTransformation) * 
      (exp(-alpha * nonLinTransformation) - 1)
    
    dtransFunRes_dparRtf <- dtransFunRes_dparScaledRtf %*% dparScaled_dpar + 
      dtransFunRes_dnonLinTrans %*% 
      dnonLinTrans_dparScaledRtf %*% 
      dparScaled_dpar 
    
    dtransFunRes_dparRtf
  } else {
    transientFunctionRes
  }
}
