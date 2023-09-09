#' Calculate RTF results for multiple time points
#'
#' @description Apply RTF with defined parameters for defined time points
#' @return Vector of predicted quantitative values resulting from applying RTF
#' with the specified parameters to time points given in t.
#' @param par Named vector of the parameter values used for RTF.
#' @param t Vector of time points
#' @param fixed Vector of fixed parameters, which are used to overwrite values
#' in par, if they are non-NAs
#' @param modus String indicating if modus 'RetardedTransientDynamics' or
#' 'ImmediateResponseFunction' should be used
#' @export getTransientFunctionResult
#' @examples
#' par <- c(tau_1 = 1.00, tau_2 = 1.00, A_sus = 1.05,
#'          A_trans = 3.05, p_0 = -0.28, T_shift = -1)
#' t <- c(0, 0.71, 1.42, 2.14, 2.85, 3.57, 4.28, 5, 5.71, 6.42,
#'              7.14, 7.85, 8.57, 9.28, 10)
#' fixed <- c(signum_TF = 1, tau_1 = NA, tau_2 = 2.5, A_sus = NA, A_trans = NA,
#'            p_0 = NA, T_shift = NA)
#' modus <- "RetardedTransientDynamics"
#' y <- getTransientFunctionResult(par = par,
#'                            t = t,
#'                            fixed = fixed,
#'                            modus = modus)
#' plot(t, y)

getTransientFunctionResult <- function(par,
                                       t,
                                       d = NULL,
                                       fixed,
                                       modus = "RetardedTransientDynamics") {

  for (v in 1:length(par)) assign(names(par)[v], par[[v]])

  # Fixed parameters will overwrite the values in par
  for (v in 1:length(fixed)) {
    if (!is.na(fixed[[v]])) assign(names(fixed)[v], fixed[[v]])
  }

  scaleRes <- scaleTimeParameter(timeParam=t, maxVal = max(t))
  t_prime <- scaleRes$timeParam
  maxVal <- scaleRes$maxVal
  
  if (modus == "DoseDependentRetardedTransientDynamics") {
    A_sus <- hillEquation(d = d, M = M_Asus, h = h_Asus, K = K_Asus)
    A_trans <- hillEquation(d = d, M = M_Atrans, h = h_Atrans, K = K_Atrans)
    # rec_tau_1_d <-  hillEquation(d = d, M = M_rec_tau_1, 
    #                                       h = h_rec_tau_1, K = K_rec_tau_1)
    # rec_tau_2_d <-  hillEquation(d = d, M = M_rec_tau_2, 
    #                                       h = h_rec_tau_2, K = K_rec_tau_2)
    tau_1 <- hillEquationReciprocal(d = d, M = M_tau1, 
                                      h = h_tau1, K = K_tau1)
    tau_2 <- hillEquationReciprocal(d = d, M = M_tau2, 
                                      h = h_tau2, K = K_tau2)
    T_shift <- hillEquationReciprocal(d = d, M = M_Tshift, 
                                        h = h_Tshift, K = K_Tshift)
  }

  # scaling everything with time as phys. unit
  T_shift <- scaleTimeParameter(timeParam = T_shift, maxVal = maxVal)$timeParam
  tau_1 <- scaleTimeParameter(timeParam = tau_1, maxVal = maxVal)$timeParam
  tau_2 <- scaleTimeParameter(timeParam = tau_2, maxVal = maxVal)$timeParam

  nonLinTransformation <- log10(10^t_prime + 10^T_shift) - log10(1 + 10^T_shift)

  if (modus == "ImmediateResponseFunction") {
    Signal_sus <- A_sus * (1-exp(-t_prime / tau_1))
    Signal_trans <- A_trans * (1 - exp(-t_prime / tau_1)) *
      exp(-t_prime / tau_2)
    transientFunctionRes <- signum_TF * Signal_sus +
      signum_TF * Signal_trans + p_0
  } else if (modus %in% c("RetardedTransientDynamics", 
                          "DoseDependentRetardedTransientDynamics")) {
    Signal_sus <- A_sus * (1 - exp(-nonLinTransformation / tau_1))
    Signal_trans <- A_trans * (1 - exp(-nonLinTransformation / tau_1)) *
      exp(-nonLinTransformation / tau_2)
    transientFunctionRes <- signum_TF * Signal_sus +
      signum_TF * Signal_trans + p_0
  } 
  if(sum(is.infinite(transientFunctionRes))>0)
    print(transientFunctionRes)
  transientFunctionRes
}
