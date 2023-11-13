#' Calculate sum of the sustained part of the RTF and offset b
#'
#' @description Calculate sum of the sustained part of the RTF and offset b
#' for a defined time point.
#' @return Sum of the sustained part of the RTF and offset b for a defined
#' time point t.
#' @param alphaInv alphaInv
#' @param A A
#' @param b b
#' @param tau tau
#' @param signum_TF signum_TF
#' @param t Time point
#' @export getSignalSusPlusOffset
#' @examples
#' getSignalSusPlusOffset(
#'         alphaInv = 1.00,
#'         A = 1.05,
#'         b = -0.28, tau = -1, signum_TF = 1,
#'         t = 0.6)

getSignalSusPlusOffset <- function(
    alphaInv, A, b, tau, signum_TF, t) {
  nonLinTransformation <-
    log10(10^t + 10^tau) - log10(1 + 10^tau)
  Signal_sus <- A * (1 - exp(-nonLinTransformation / alphaInv))
  signum_TF * Signal_sus + b
}
