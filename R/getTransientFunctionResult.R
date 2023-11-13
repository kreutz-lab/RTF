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
#' alphaInv, and gammaInv should be scaled
#' @export getTransientFunctionResult
#' @examples
#' par <- c(alphaInv = 1.00, gammaInv = 1.00, A = 1.05,
#'          B = 3.05, b = -0.28, tau = -1)
#' t <- c(0, 0.71, 1.42, 2.14, 2.85, 3.57, 4.28, 5, 5.71, 6.42,
#'              7.14, 7.85, 8.57, 9.28, 10)
#' fixed <- c(signum_TF = 1, alphaInv = NA, gammaInv = 2.5, A = NA, B = NA,
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
                                       scale = TRUE) {

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
                         params = c(M_alphaInv = M_alphaInv, 
                                   h_alphaInv = h_alphaInv, 
                                   K_alphaInv = K_alphaInv, 
                                   M_gammaInv = M_gammaInv, 
                                   h_gammaInv = h_gammaInv, 
                                   K_gammaInv = K_gammaInv,
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
    alphaInv <- df$alphaInv
    gammaInv <- df$gammaInv
    tau <- df$tau
  }
  
  if (scale) {
    # scaling everything with time as phys. unit
    tau <- scaleTimeParameter(timeParam = tau, maxVal = maxVal)$timeParam
    alphaInv <- scaleTimeParameter(timeParam = alphaInv, maxVal = maxVal)$timeParam
    gammaInv <- scaleTimeParameter(timeParam = gammaInv, maxVal = maxVal)$timeParam
  }

  nonLinTransformation <- log10(10^t_prime + 10^tau) - log10(1 + 10^tau)

  if (modus == "ImmediateResponseFunction") {
    Signal_sus <- A * (1-exp(-t_prime / alphaInv))
    Signal_trans <- B * (1 - exp(-t_prime / alphaInv)) *
      exp(-t_prime / gammaInv)
    transientFunctionRes <- signum_TF * Signal_sus +
      signum_TF * Signal_trans + b
  } else if (modus %in% c("RetardedTransientDynamics", 
                          "DoseDependentRetardedTransientDynamics")) {
    Signal_sus <- A * (1 - exp(-nonLinTransformation / alphaInv))
    Signal_trans <- B * (1 - exp(-nonLinTransformation / alphaInv)) *
      exp(-nonLinTransformation / gammaInv)
    transientFunctionRes <- signum_TF * Signal_sus +
      signum_TF * Signal_trans + b
  } 
  if(sum(is.infinite(transientFunctionRes))>0)
    print(transientFunctionRes)
  transientFunctionRes
}
