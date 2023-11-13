#' Calculate sum of the transient part of the RTF and offset b
#'
#' @description Calculate sum of the transient part of the RTF and offset b
#' for a defined time point.
#' @return Sum of the transient part of the RTF and offset b for a defined
#' time point t.
#' @param alpha alpha
#' @param gamma gamma
#' @param B B
#' @param b b
#' @param tau tau
#' @param signum_TF signum_TF
#' @param t Time point
#' @export getSignalTransPlusOffset
#' @examples
#' getSignalTransPlusOffset(
#'         alpha = 1.00, gamma = 1.00,
#'         B = 3.05,
#'         b = -0.28, tau = -1, signum_TF = 1,
#'         t = 0.6)

getSignalTransPlusOffset <- function(
    alpha, gamma, B, b, tau, signum_TF, t) {
  nonLinTransformation <-
    log10(10^t + 10^tau) - log10(1 + 10^tau)
  Signal_trans <-
    B * (1 - exp(- alpha * nonLinTransformation)) *
    exp(- gamma * nonLinTransformation)
  
  signum_TF * Signal_trans + b
}
