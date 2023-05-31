#' @description Calculate sum of nonlinear transformation and offset p_0
#' for a defined time point.
#' @return Sum of nonlinear transformation and offset p_0 for a defined
#' time point t_prime.
#' @param p_0 p_0
#' @param T_shift T_shift
#' @param t_prime timepoint
#' @export getNonLinTransformationPlusOffset
#' @examples
#' getNonLinTransformationPlusOffset(
#'         p_0 = -0.28,
#'         T_shift = -1,
#'         t_prime = 0.6)

getNonLinTransformationPlusOffset <- function(p_0, T_shift, t_prime) {
  nonLinTransformation <- log10(10^t_prime+10^T_shift)-log10(1+10^T_shift)
  nonLinTransformation + p_0
}
