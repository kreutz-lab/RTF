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

getTransientFunctionResult <- function(par = c(),
                                       t = NULL,
                                       d = NULL,
                                       fixed = NA,
                                       modus = "RetardedTransientDynamics",
                                       scale = TRUE, calcGradient = T) {

  for (v in 1:length(par)) assign(names(par)[v], par[[v]])

  # Fixed parameters will overwrite the values in par
  for (v in 1:length(fixed)) {
    if (!is.na(fixed[[v]])) assign(names(fixed)[v], fixed[[v]])
  }

  if (scale) {
    scaleRes <- scaleTimeParameter(timeParam = t, maxVal = max(t))
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
                                   K_tau = K_tau), )
    A <- df$A
    B <- df$B
    alpha <- df$alpha
    gamma <- df$gamma
    tau <- df$tau
  }
  
   
  if (scale) {
    # scaling everything with time as phys. unit
    tau <- scaleTimeParameter(timeParam = tau, maxVal = maxVal)$timeParam
    alpha <- scaleTimeParameter(timeParam = alpha, maxVal = 1/maxVal)$timeParam
    gamma <- scaleTimeParameter(timeParam = gamma, maxVal = 1/maxVal)$timeParam
  }

  nonLinTransformation <- log10(10^t_prime + 10^tau) - log10(1 + 10^tau)

  if (modus == "ImmediateResponseFunction") {
    Signal_sus <- A * (1-exp(- alpha * t_prime))
    Signal_trans <- B * (1 - exp(- alpha * t_prime)) *
      exp(- gamma * t_prime)
    
    transientFunctionRes <- signum_TF * Signal_sus +
      signum_TF * Signal_trans + b
  } else if (modus %in% c("RetardedTransientDynamics", 
                          "DoseDependentRetardedTransientDynamics")) {
    Signal_sus <- A * (1 - exp(- alpha * nonLinTransformation))
    Signal_trans <- B * (1 - exp(- alpha * nonLinTransformation)) *
       exp(- gamma * nonLinTransformation)
    transientFunctionRes <- signum_TF * Signal_sus +
      signum_TF * Signal_trans + b # + (abs(alpha-gamma)/1000)
  } 
  if(sum(is.infinite(transientFunctionRes))>0)
    print(transientFunctionRes)
  
  # if(signum_TF>0){
  #   
  #   print(signum_TF)
  #   
  # }
  transientFunctionRes
}
