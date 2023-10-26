#' Calculate Dose Response RTF
#'
#' @description Calculate RTF result for a defined time point with defined
#' parameters
#' @return Quantitative values resulting from applying RTF
#' with the specified parameters to a given time point.
#' @param tau_1 tau_1
#' @param tau_2 tau_2
#' @param A_sus A_sus
#' @param A_trans A_trans
#' @param p_0 p_0
#' @param T_shift T_shift
#' @param signum_TF signum_TF
#' @param t Time point
#' @export getTransientFunctionExampleData
#' @examples
#' getDoseResponseTransientFunctionExampleData(
#'     d = 2, 
#'     M_tau1 = 1, h_tau1 = 0.5, K_tau1 = 0.3, 
#'     M_tau2 = 1, h_tau2 = 0.5, K_tau2 = 0.3, 
#'     M_Asus = 15, h_Asus = 0.5, K_Asus = 0.3, 
#'     M_Atrans = 15, h_Atrans = 0.5, K_Atrans = 0.3, 
#'     M_Tshift = 1, h_Tshift = 0.5, K_Tshift = 0.3, 
#'     p_0 = -0.3, signum_TF = 1, t = 0.6) 

getDoseResponseTransientFunctionExampleData <- function(
    d, M_tau1, h_tau1, K_tau1, M_tau2, h_tau2, K_tau2, 
    M_Asus, h_Asus, K_Asus, M_Atrans, h_Atrans, K_Atrans, 
    M_Tshift, h_Tshift, K_Tshift, p_0, signum_TF, t) {
  
  A_sus <- hillEquation(d = d, M = M_Asus, h = h_Asus, K = K_Asus)
  A_trans <- hillEquation(d = d, M = M_Atrans, h = h_Atrans, K = K_Atrans)
  tau_1 <- hillEquationReciprocal(d = d, M = M_tau1, 
                                  h = h_tau1, K = K_tau1, minval = 1e-6)
  tau_2 <- hillEquationReciprocal(d = d, M = M_tau2, 
                                  h = h_tau2, K = K_tau2, minval = 1e-6)
  T_shift <- hillEquationReciprocal(d = d, M = M_Tshift, 
                                    h = h_Tshift, K = K_Tshift)
  
  nonLinTransformation <- log10(10^t + 10^T_shift) - log10(1 + 10^T_shift)
  
  Signal_sus <- A_sus * (1 - exp(-nonLinTransformation / tau_1))
  Signal_trans <- A_trans * (1 - exp(-nonLinTransformation / tau_1)) *
    exp(-nonLinTransformation / tau_2)
  transientFunctionRes <- signum_TF * Signal_sus +
    signum_TF * Signal_trans + p_0
  
  transientFunctionRes
  
}
