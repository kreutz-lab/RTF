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
#' @param t_prime timepoint
#' @export getTransientFunctionExampleData
#' @examples
#' getTransientFunctionExampleData(
#'         tau_1 = 1.00, tau_2 = 1.00,
#'         A_sus = 1.05, A_trans = 3.05,
#'         p_0 = -0.28, T_shift = -1, signum_TF = 1,
#'         t_prime = 0.6)

getTransientFunctionExampleData <- function(
    tau_1, tau_2, A_sus, A_trans, p_0, T_shift, signum_TF, t_prime) {

  nonLinTransformation <- log10(10^t_prime+10^T_shift)-log10(1+10^T_shift)

  Signal_sus <- A_sus * (1-exp(- nonLinTransformation /tau_1))  # Warum tau_2? Taucht in Paperformel nicht auf
  Signal_trans <- A_trans * (1-exp(- nonLinTransformation /tau_1))* exp(- nonLinTransformation /tau_2)
  transientFunctionRes <- signum_TF * Signal_sus + signum_TF * Signal_trans + p_0

  transientFunctionRes
}
