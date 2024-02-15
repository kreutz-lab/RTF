#' Calculate RTF results for multiple time points
#'
#' @description Apply RTF with defined parameters for defined time points
#' @return Vector of predicted quantitative values resulting from applying RTF
#' with the specified parameters to time points given in t.
#' @param par Named vector of the parameter values used for RTF.
#' @param t Vector of time points
#' @param d Dose. Only relevant for dose-response RTF.
#' @param fixed Vector of fixed parameters, which are used to overwrite values
#' in par, if they are non-NAs
#' @param modus String indicating if modus 'RetardedTransientDynamics' or
#' 'ImmediateResponseFunction' should be used
#' @param scale Boolean, indicates if time dependent parameters t, tau,
#' alpha, and gamma should be scaled
#' @export getTransientFunctionResult
#' @examples
#' par <- c(alpha = 1.00, gamma = 1.00, A = 1.05,
#'          B = 3.05, b = -0.28, tau = -1)
#' t <- c(0, 0.71, 1.42, 2.14, 2.85, 3.57, 4.28, 5, 5.71, 6.42,
#'              7.14, 7.85, 8.57, 9.28, 10)
#' fixed <- c(signum_TF = 1, alpha = NA, gamma = 2.5, A = NA, B = NA,
#'            b = NA, tau = NA)
#' modus <- "RetardedTransientDynamics"
#' y <- getTransientFunctionResult(par = par,
#'                            t = t,
#'                            fixed = fixed,
#'                            modus = modus)
#' plot(t, y)
#' 
#' d = c(2, 6)
#' par = c(M_alpha = 1, h_alpha = 2, K_alpha = 2,
#'         M_gamma = 1, h_gamma = 3, K_gamma = 2,
#'         M_A = 4, h_A = 2, K_A = 1,
#'         M_B = 2, h_B = 3, K_B = 1,
#'         M_tau = 2, h_tau = 3, K_tau = 2)
#' t <- c(0, 0.71, 1.42, 2.14, 2.85, 3.57, 4.28, 5, 5.71, 6.42,
#'        7.14, 7.85, 8.57, 9.28, 10)
#' fixed <- c(signum_TF = 1, alpha = NA, gamma = 2.5, A = NA, B = NA,
#'            b = NA, tau = NA)

getTransientFunctionResult <- function(par = c(),
                                       t = NULL,
                                       d = NULL,
                                       fixed = NA,
                                       modus = "RetardedTransientDynamics",
                                       scale = TRUE, calcGradient = F) {

  for (v in 1:length(par)) assign(names(par)[v], par[[v]])

  # Fixed parameters will overwrite the values in par
  dpar_dparVorFix <- matrix(0, nrow = length(par), ncol = length(par))
  rownames(dpar_dparVorFix) <- colnames(dpar_dparVorFix) <- names(par)
  diag(dpar_dparVorFix) <- 1
  for (v in 1:length(fixed)) {
    if (!is.na(fixed[[v]])) {
      assign(names(fixed)[v], fixed[[v]])
      # if (sum(names(fixed)[v] == names(par)) > 0)
      #   dpar_dparVorFix[names(fixed)[v]==names(par),
      #                   names(fixed)[v]==names(par),] <- 0
    }
  }
  
  overlap <- intersect(names(fixed[!is.na(fixed)]), names(par)) 
  dpar_dparVorFix[names(par) %in% overlap, names(par) %in% overlap] <- 0

  if (scale) {
    scaleRes <- scaleTimeParameter(timeParam = t, maxVal = 10/max(t))
    t_prime <- scaleRes$timeParam
    maxVal <- scaleRes$maxVal
  } else {
    t_prime <- t
  }
  
  if (modus == "DoseDependentRetardedTransientDynamics") {
    df <- getHillResults(d = d, 
                         params = c(M_alpha = M_alpha, 
                                    h_alpha = h_alpha, 
                                    K_alpha = K_alpha, 
                                    M_gamma = M_gamma, 
                                    h_gamma = h_gamma, 
                                    K_gamma = K_gamma,
                                    M_A = M_A, 
                                    h_A = h_A, 
                                    K_A = K_A, 
                                    M_B = M_B, 
                                    h_B = h_B, 
                                    K_B = K_B,
                                    M_tau = M_tau, 
                                    h_tau = h_tau, 
                                    K_tau = K_tau))
    A <- df$A
    B <- df$B
    alpha <- df$alpha
    gamma <- df$gamma
    tau <- df$tau

    # if(calcGradient)
      df_dpar <- getHillResults(d = d, 
                           params = c(M_alpha = M_alpha, 
                                      h_alpha = h_alpha, 
                                      K_alpha = K_alpha, 
                                      M_gamma = M_gamma, 
                                      h_gamma = h_gamma, 
                                      K_gamma = K_gamma,
                                      M_A = M_A, 
                                      h_A = h_A, 
                                      K_A = K_A, 
                                      M_B = M_B, 
                                      h_B = h_B, 
                                      K_B = K_B,
                                      M_tau = M_tau, 
                                      h_tau = h_tau, 
                                      K_tau = K_tau), gradientNames = names(par))
    dA_dpar <- df_dpar$A
    dB_dpar <- df_dpar$B
    dalpha_dpar <- df_dpar$alpha
    dgamma_dpar <- df_dpar$gamma
    dtau_dpar <- df_dpar$tau
  }

   
  # scaling everything with time as phys. unit

  dparScaled_dpar <- matrix(0,nrow=length(par),ncol=length(par))
  diag(dparScaled_dpar) <- 1
  if (scale) {
    if(calcGradient){
      dtau_dtau <- scaleTimeParameter(timeParam = c(tau=tau), maxVal = maxVal, gradientNames = names(par))$timeParam
      dalpha_dalpha <- scaleTimeParameter(timeParam = c(alpha=alpha), maxVal = 1/maxVal, gradientNames = names(par))$timeParam
      dgamma_dgamma <- scaleTimeParameter(timeParam = c(gamma=gamma), maxVal = 1/maxVal, gradientNames = names(par))$timeParam
      
      if(modus == "DoseDependentRetardedTransientDynamics") {
        dtau_dpar <- dtau_dtau * dtau_dpar[,"tau"]
        dalpha_dpar <- dalpha_dalpha * dalpha_dpar[,"alpha"]
        dgamma_dpar <- dgamma_dgamma * dgamma_dpar[,"gamma"]
      } else {
        dtau_dpar <- dtau_dtau 
        dalpha_dpar <- dalpha_dalpha
        dgamma_dpar <- dgamma_dgamma 
      }
      
      rownames(dparScaled_dpar) <- names(par)
      colnames(dparScaled_dpar) <- names(par)
      dparScaled_dpar["tau",]   <- dtau_dpar
      dparScaled_dpar["alpha",] <- dalpha_dpar
      dparScaled_dpar["gamma",] <- dgamma_dpar
      
    }
    tau <- scaleTimeParameter(timeParam = c(tau=tau), maxVal = maxVal)$timeParam
    alpha <- scaleTimeParameter(timeParam = c(alpha=alpha), maxVal = 1/maxVal)$timeParam
    gamma <- scaleTimeParameter(timeParam = c(gamma=gamma), maxVal = 1/maxVal)$timeParam
  }

  if (modus == "ImmediateResponseFunction") {
    
    transientFunctionRes <- signum_TF * A * (1-exp(- alpha * t_prime)) + signum_TF * B * (1 - exp(- alpha * t_prime)) * exp(- gamma * t_prime) + b
    dtransFunRes_dpar <- matrix(0,nrow=length(transientFunctionRes),ncol=length(par))
    colnames(dtransFunRes_dpar) <- names(par)
    dtransFunRes_dpar[,"alpha"] <- A*signum_TF*t_prime*exp(-alpha*t_prime) + B*signum_TF*t_prime*exp(-alpha*t_prime)*exp(-gamma*t_prime)
    dtransFunRes_dpar[,"gamma"] <- B*signum_TF*t_prime*exp(-gamma*t_prime)*(exp(-alpha*t_prime) - 1)
    dtransFunRes_dpar[,"b"] <- 1
    dtransFunRes_dpar[,"A"] <- -signum_TF*(exp(-alpha*t_prime) - 1)
    dtransFunRes_dpar[,"B"] <- -signum_TF*exp(-gamma*t_prime)*(exp(-alpha*t_prime) - 1)
    
  } else if (modus %in% c("RetardedTransientDynamics", 
                          "DoseDependentRetardedTransientDynamics")) {
    nonLinTransformation <- log10(10^t_prime + 10^tau) - log10(1 + 10^tau)
    
    dnonLinTrans_dpar <- matrix(0,nrow=length(nonLinTransformation),ncol=length(par))
    colnames(dnonLinTrans_dpar) <- names(par)
    dnonLinTrans_dpar[,"tau"] <- 10^tau/(10^t_prime + 10^tau) - 10^tau/(10^tau + 1)

    transientFunctionRes <- signum_TF*A*(1-exp(-alpha*nonLinTransformation)) + signum_TF*B*(1-exp(-alpha*nonLinTransformation))*exp(-gamma*nonLinTransformation) + b
    dtransFunRes_dnonLinTrans <- matrix(0,nrow=length(transientFunctionRes),ncol=length(nonLinTransformation))
    diag(dtransFunRes_dnonLinTrans) <- A*alpha*signum_TF*exp(-alpha*nonLinTransformation) + B*gamma*signum_TF*exp(-gamma*nonLinTransformation)*(exp(-alpha*nonLinTransformation) - 1) + B*alpha*signum_TF*exp(-alpha*nonLinTransformation)*exp(-gamma*nonLinTransformation)
    dtransFunRes_dpar <- matrix(0,nrow=length(transientFunctionRes),ncol=length(par))
    colnames(dtransFunRes_dpar) <- names(par)
    dtransFunRes_dpar[,"alpha"] <- A*signum_TF*nonLinTransformation*exp(-alpha*nonLinTransformation) + B*signum_TF*nonLinTransformation*exp(-alpha*nonLinTransformation)*exp(-gamma*nonLinTransformation)
    dtransFunRes_dpar[,"gamma"] <- B*signum_TF*nonLinTransformation*exp(-gamma*nonLinTransformation)*(exp(-alpha*nonLinTransformation) - 1)
    dtransFunRes_dpar[,"b"] <- 1
    dtransFunRes_dpar[,"A"] <- -signum_TF*(exp(-alpha*nonLinTransformation) - 1)
    dtransFunRes_dpar[,"B"] <- -signum_TF*exp(-gamma*nonLinTransformation)*(exp(-alpha*nonLinTransformation) - 1)
    
    dtransFunRes_dpar <- dtransFunRes_dpar + dtransFunRes_dnonLinTrans %*% dnonLinTrans_dpar ## Bock
  } 
  
  if (modus == "DoseDependentRetardedTransientDynamics") {
    # dtransFunRes_dpar dtransFunRes_dpar[,"A"]*dA_dpar
    # 
    # dB_dpar <- df_dpar$B
    # dalpha_dpar <- df_dpar$alpha
    # dgamma_dpar <- df_dpar$gamma
    # dtau_dpar
  }
  
  if(sum(is.infinite(transientFunctionRes))>0)
    print(transientFunctionRes)
  
  # if(signum_TF>0){
  #   
  #   print(signum_TF)
  #   
  # }
  if(!calcGradient)
    # set derivates of fixed parameters to zero
    transientFunctionRes 
  else
    return(dtransFunRes_dpar %*% dpar_dparVorFix) # consider fixing of parameters
  
}
