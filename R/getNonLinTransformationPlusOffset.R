#' Calculate sum of nonlinear transformation and offset p_0
#'
#' @description Calculate sum of nonlinear transformation and offset p_0
#' for a defined time point.
#' @return Sum of nonlinear transformation and offset p_0 for a defined
#' time point t.
#' @param p_0 p_0
#' @param T_shift T_shift
#' @param t Time point
#' @export getNonLinTransformationPlusOffset
#' @examples
#' getNonLinTransformationPlusOffset(
#'         p_0 = -0.28,
#'         T_shift = -1,
#'         t = 0.6)

getNonLinTransformationPlusOffset <- function(p_0, T_shift, t) {
  nonLinTransformation <- log10(10^t + 10^T_shift) - log10(1 + 10^T_shift)
  nonLinTransformation + p_0
}
