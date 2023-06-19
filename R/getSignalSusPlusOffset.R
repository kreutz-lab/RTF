#' Calculate sum of the sustained part of the RTF and offset p_0
#'
#' @description Calculate sum of the sustained part of the RTF and offset p_0
#' for a defined time point.
#' @return Sum of the sustained part of the RTF and offset p_0 for a defined
#' time point t.
#' @param tau_1 tau_1
#' @param A_sus A_sus
#' @param p_0 p_0
#' @param T_shift T_shift
#' @param signum_TF signum_TF
#' @param t Time point
#' @export getSignalSusPlusOffset
#' @examples
#' getSignalSusPlusOffset(
#'         tau_1 = 1.00,
#'         A_sus = 1.05,
#'         p_0 = -0.28, T_shift = -1, signum_TF = 1,
#'         t = 0.6)

getSignalSusPlusOffset <- function(
    tau_1, A_sus, p_0, T_shift, signum_TF, t) {
  nonLinTransformation <-
    log10(10^t + 10^T_shift) - log10(1 + 10^T_shift)
  Signal_sus <- A_sus * (1 - exp(-nonLinTransformation / tau_1))
  signum_TF * Signal_sus + p_0
}
