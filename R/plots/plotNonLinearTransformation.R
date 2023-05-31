#' @description Plot sum of nonlinear transformation and offset p_0
#' together with experimental data points y.
#' @return ggplot object showing sum of nonlinear transformation and offset p_0
#' together with experimental data points y.
#' @param p_0 p_0
#' @param T_shift T_shift
#' @param y Experimental outcome for time points (corresponds to y column in
#' experimental data frame)
#' @param t_prime timepoints
#' @export plotSignalSus
#' @examples
#' gg <- plotNonLinearTransformation(
#'        p_0 = -0.28, T_shift = -1,
#'        y = c(0.45, 0.96, 1.13, 1.1, 0.9, 0.76, 0.78),
#'        t_prime = c(0, 0.7, 1.2, 1.55, 2.3, 7.45, 10))

plotNonLinearTransformation <- function(p_0, T_shift, y, t_prime) {
  xi <- seq(0, max(t_prime), length.out = 1000)

  ggplot(data.frame(t_prime = t_prime, y = y), aes(x=t_prime, y=y)) +
    geom_point(alpha=0.5) +
    ggtitle("NonLinTransformation + p_0") +
    geom_line(data=data.frame(x = xi,
                              y = getNonLinTransformationPlusOffset(
                                t_prime = xi,
                                T_shift = T_shift,
                                p_0 = p_0)), aes(x=x,y=y)) +
    theme_bw()
}
