#' Calculate sum of nonlinear transformation and offset b
#'
#' @description Calculate sum of nonlinear transformation and offset b
#' for a defined time point.
#' @return Sum of nonlinear transformation and offset b for a defined
#' time point t.
#' @param b b
#' @param tau tau
#' @param t Time point
#' @export getNonLinTransformationPlusOffset
#' @examples
#' getNonLinTransformationPlusOffset(
#'         b = -0.28,
#'         tau = -1,
#'         t = 0.6)

getNonLinTransformationPlusOffset <- function(b, tau, t) {
  nonLinTransformation <- log10(10^t + 10^tau) - log10(1 + 10^tau)
  nonLinTransformation + b
}
