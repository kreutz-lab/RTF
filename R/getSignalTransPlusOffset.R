#' Calculate sum of the transient part of the RTF and offset b
#'
#' @description Calculate sum of the transient part of the RTF and offset b
#' for a defined time point.
#' @return Sum of the transient part of the RTF and offset b for a defined
#' time point t.
#' @param alphaInv alphaInv
#' @param gammaInv gammaInv
#' @param B B
#' @param b b
#' @param tau tau
#' @param signum_TF signum_TF
#' @param t Time point
#' @export getSignalTransPlusOffset
#' @examples
#' getSignalTransPlusOffset(
#'         alphaInv = 1.00, gammaInv = 1.00,
#'         B = 3.05,
#'         b = -0.28, tau = -1, signum_TF = 1,
#'         t = 0.6)

getSignalTransPlusOffset <- function(
    alphaInv, gammaInv, B, b, tau, signum_TF, t) {
  nonLinTransformation <-
    log10(10^t + 10^tau) - log10(1 + 10^tau)
  Signal_trans <-
    B * (1 - exp(-nonLinTransformation / alphaInv)) *
    exp(-nonLinTransformation / gammaInv)
  signum_TF * Signal_trans + b
}
