#' @description Calculate sum of the transient part of the RTF and offset p_0
#' for a defined time point.
#' @return Sum of the transient part of the RTF and offset p_0 for a defined
#' time point t_prime.
#' @param tau_1 tau_1
#' @param tau_2 tau_2
#' @param A_trans A_trans
#' @param p_0 p_0
#' @param T_shift T_shift
#' @param signum_TF signum_TF
#' @param t_prime timepoint
#' @export getSignalTransPlusOffset
#' @examples
#' getSignalTransPlusOffset(
#'         tau_1 = 1.00, tau_2 = 1.00,
#'         A_trans = 3.05,
#'         p_0 = -0.28, T_shift = -1, signum_TF = 1,
#'         t_prime = 0.6)

getSignalTransPlusOffset <- function(
    tau_1, tau_2, A_trans, p_0, T_shift, signum_TF, t_prime) {
  nonLinTransformation <-
    log10(10^t_prime + 10^T_shift) - log10(1 + 10^T_shift)
  Signal_trans <-
    A_trans * (1 - exp(-nonLinTransformation / tau_1)) *
    exp(-nonLinTransformation / tau_2)
  signum_TF * Signal_trans + p_0
}
