#' Calculate RTF results for multiple time points
#'
#' @description Apply RTF with defined parameters for defined time points
#' @return Vector of predicted quantitative values resulting from applying RTF
#' with the specified parameters to time points given in t.
#' @param par Named vector of the parameter values used for RTF.
#' @param t Vector of time points
#' @param modus String indicating if modus 'timeDependent' or
#' 'ImmediateResponseFunction' should be used
#' @param scale Boolean, indicates if time dependent parameters t, tau,
#' alpha, and gamma should be scaled
#' @param calcGradient Boolean indicating if gradient should be calculated
#' (Default: FALSE)
#' @param dpar_dparVorFix description
#' @param hillGradient description
#' @export getTransientFunctionResult
#' @examples
#' par <- c(alpha = 1.00, gamma = 1.00, A = 1.05,
#'          B = 3.05, b = -0.28, tau = -1)
#' t <- c(0, 0.71, 1.42, 2.14, 2.85, 3.57, 4.28, 5, 5.71, 6.42,
#'              7.14, 7.85, 8.57, 9.28, 10)
#' # fixed <- c(signum_TF = 1, alpha = NA, gamma = 2.5, A = 3, B = NA,
#' #          b = NA, tau = NA)
#' modus <- "timeDependent"
#' y <- getTransientFunctionResult(par = par,
#'                            t = t,
#'                            modus = modus)
#' plot(t, y)
#' 
#' # d = 4
#' par = c(M_alpha = 1, h_alpha = 2, K_alpha = 2,
#'         M_gamma = 1, h_gamma = 3, K_gamma = 2,
#'         M_A = 4, h_A = 2, K_A = 1,
#'         M_B = 2, h_B = 3, K_B = 1,
#'         M_tau = 2, h_tau = 3, K_tau = 2, b = 1)
#' t <- c(0, 0.71, 1.42, 2.14, 2.85, 3.57, 4.28, 5, 5.71, 6.42,
#'        7.14, 7.85, 8.57, 9.28, 10)
#' # fixed <- c(par * NA, signum_TF = 1, h_gamma = 4)
#'  y <- getTransientFunctionResult(par = par, # d = d,
#'              t = t, 
#'              # fixed = fixed, 
#'              modus = "doseDependent")

getTransientFunctionResult <- function(rtfPar = c(),
                                       t = NULL,
                                       # d = NULL,
                                       # fixed = NA,
                                       modus = "timeDependent",
                                       scale = TRUE, 
                                       calcGradient = FALSE,
                                       dpar_dparVorFix = NULL,
                                       hillGradient = NULL) {
  
  # for (v in 1:length(par)) assign(names(par)[v], par[[v]])

  # # Fixed parameters will overwrite the values in par
  # dpar_dparVorFix <- matrix(0, nrow = length(par), ncol = length(par))
  # rownames(dpar_dparVorFix) <- colnames(dpar_dparVorFix) <- names(par)
  # diag(dpar_dparVorFix) <- 1
  # 
  # overlap <- intersect(names(fixed[!is.na(fixed)]), names(par)) 
  # dpar_dparVorFix[names(par) %in% overlap, names(par) %in% overlap] <- 0
  # 
  # for (v in 1:length(fixed)) {
  #   if (!is.na(fixed[[v]])# & names(fixed)[v] %in% names(par)
  #       ) {
  #     # assign(names(fixed)[v], fixed[[v]])
  #     par[names(fixed)[v]] <- fixed[[v]]
  #   }
  # }
  
  for (v in 1:length(par)) assign(names(par)[v], par[[v]])

  if (scale) {
    scaleRes <- scaleTimeParameter(timeParam = t, maxVal = 10/max(t))
    t_prime <- scaleRes$timeParam
    maxVal <- scaleRes$maxVal
  } else {
    t_prime <- t
  }
  
  rtfParams <- c("A", "B", "alpha", "gamma", "tau")
  
  # if (modus == "doseDependent") {
  #   getRTFparamsFromHill <- function(d, par) {
  #     df <- getHillResults(d = d, 
  #                          params = par)
  #     
  #     # rtfParams <- names(df) ## ugly point in the code 
  # 
  #     for(el in rtfParams)    
  #       assign(el,df[[el]])
  # 
  #     # if(calcGradient)
  #       hillGradient <- getHillResults(d = d, # drtfParams_dpar
  #                            params = par, calcGradient = TRUE)
  #   }
  # }

  # scaling everything with time as phys. unit
  dparScaled_dpar <- matrix(0, nrow = length(par), ncol = length(par))
  diag(dparScaled_dpar) <- 1
  if (scale) {
    if (calcGradient) {
      dtau_dtau <- scaleTimeParameter(
        timeParam = c(tau = tau), 
        maxVal = maxVal, 
        gradientNames = "tau")$timeParam
      
      dalpha_dalpha <- scaleTimeParameter(
        timeParam = c(alpha = alpha), 
        maxVal = 1/maxVal, 
        gradientNames = "alpha")$timeParam
      
      dgamma_dgamma <- scaleTimeParameter(
        timeParam = c(gamma = gamma), 
        maxVal = 1/maxVal, 
        gradientNames = "gamma")$timeParam
      
      if (modus == "doseDependent") {
        dtau_dpar <- dtau_dtau * dtau_dpar[,"tau"]
        dalpha_dpar <- dalpha_dalpha * dalpha_dpar[,"alpha"]
        dgamma_dpar <- dgamma_dgamma * dgamma_dpar[,"gamma"]
      } else {
        dtau_dpar <- dtau_dtau 
        dalpha_dpar <- dalpha_dalpha
        dgamma_dpar <- dgamma_dgamma 
      }
      
      rownames(dparScaled_dpar) <- rtfParams
      colnames(dparScaled_dpar) <- names(par)
      dparScaled_dpar["tau",]   <- dtau_dpar
      dparScaled_dpar["alpha",] <- dalpha_dpar
      dparScaled_dpar["gamma",] <- dgamma_dpar
      
    }
    tau <- scaleTimeParameter(timeParam = c(tau=tau), maxVal = maxVal)$timeParam
    alpha <- scaleTimeParameter(
      timeParam = c(alpha = alpha), maxVal = 1/maxVal)$timeParam
    gamma <- scaleTimeParameter(
      timeParam = c(gamma = gamma), maxVal = 1/maxVal)$timeParam
  }

  nonLinTransformation <- log10(10^t_prime + 10^tau) - log10(1 + 10^tau)
  
  dnonLinTrans_dparRtf <- matrix(0, 
                                 nrow = length(nonLinTransformation),
                                 ncol = length(rtfParams))
  
  colnames(dnonLinTrans_dparRtf) <- rtfParams
  dnonLinTrans_dparRtf[, "tau"] <- 
    10^tau / (10^t_prime + 10^tau) - 10^tau/(10^tau + 1)

  transientFunctionRes <- 
    signum_TF*A * (1 - exp(-alpha * nonLinTransformation)) + 
    signum_TF * B * (1 - exp(-alpha * nonLinTransformation)) * 
    exp(-gamma * nonLinTransformation) + b
  
  dtransFunRes_dnonLinTrans <- matrix(0, 
                                      nrow = length(transientFunctionRes), 
                                      ncol = length(nonLinTransformation))
  diag(dtransFunRes_dnonLinTrans) <- A * alpha * signum_TF * 
    exp(-alpha * nonLinTransformation) + B * gamma * signum_TF * 
    exp(-gamma*nonLinTransformation) * 
    (exp(-alpha * nonLinTransformation) - 1) + 
    B * alpha * signum_TF * exp(-alpha * nonLinTransformation) * 
    exp(-gamma * nonLinTransformation)
  
  dtransFunRes_dparRtf <- matrix(0, 
                                 nrow = length(transientFunctionRes), 
                                 ncol = length(rtfParams))
  colnames(dtransFunRes_dparRtf) <- rtfParams
  
  dtransFunRes_dparRtf[,"alpha"] <- A * signum_TF * nonLinTransformation * 
    exp(-alpha * nonLinTransformation) + 
    B * signum_TF * nonLinTransformation * exp(-alpha * nonLinTransformation) * 
    exp(-gamma * nonLinTransformation)
  
  dtransFunRes_dparRtf[,"gamma"] <- 
    B * signum_TF * nonLinTransformation * exp(-gamma * nonLinTransformation) * 
    (exp(-alpha * nonLinTransformation) - 1)
  dtransFunRes_dparRtf[,"b"] <- 1
  
  dtransFunRes_dparRtf[,"A"] <- 
    -signum_TF * (exp(-alpha * nonLinTransformation) - 1)
  
  dtransFunRes_dparRtf[,"B"] <- 
    -signum_TF * exp(-gamma * nonLinTransformation) * 
    (exp(-alpha * nonLinTransformation) - 1)
  
  dtransFunRes_dparRtf <- dtransFunRes_dparRtf + 
    dtransFunRes_dnonLinTrans %*% dnonLinTrans_dparRtf ## Bock
  
  # dtransFunRes_dpar <- dtransFunRes_dparRtf %*% hillGradient # TODO dtransFunRes_dparRtf %*% drtfParams_dpar 

  dtransFunRes_dparRtf
  
  if (modus == "doseDependent") {
    modus
    # dtransFunRes_dpar dtransFunRes_dpar[,"A"]*dA_dpar
    # 
    # dB_dpar <- df_dpar$B
    # dalpha_dpar <- df_dpar$alpha
    # dgamma_dpar <- df_dpar$gamma
    # dtau_dpar
  }
  
  if(sum(is.infinite(transientFunctionRes)) > 0)
    print(transientFunctionRes)
  
  if(!calcGradient)
    # set derivates of fixed parameters to zero
    transientFunctionRes 
  else
    return(dtransFunRes_dpar %*% dpar_dparVorFix) # consider fixing of parameters
}
