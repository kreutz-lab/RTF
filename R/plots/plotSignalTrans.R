#' @description Plot sum of the transient part of the RTF and offset p_0
#' together with experimental data points y.
#' @return ggplot object showing transient part of the RTF and offset p_0
#' together with experimental data points y.
#' @param tau_1 tau_1
#' @param tau_2 tau_2
#' @param A_trans A_trans
#' @param p_0 p_0
#' @param T_shift T_shift
#' @param signum_TF signum_TF
#' @param y Experimental outcome for time points (corresponds to y column in
#' experimental data frame)
#' @param t_prime timepoints
#' @export plotSignalTrans
#' @examples
#' gg <- plotSignalTrans(
#'        tau_1 = 1.00, tau_2 = 1.00,
#'        A_trans = 3.05,
#'        p_0 = -0.28, T_shift = -1, signum_TF = 1,
#'        y = c(0.45, 0.96, 1.13, 1.1, 0.9, 0.76, 0.78),
#'        t_prime = c(0, 0.7, 1.2, 1.55, 2.3, 7.45, 10))

plotSignalTrans <- function(
    tau_1, tau_2, A_trans, p_0, T_shift, signum_TF, y, t_prime) {
  xi <- seq(0, max(t_prime), length.out = 1000)

  ggplot2::ggplot(data.frame(t_prime = t_prime, y = y),
                  ggplot2::aes(x = t_prime, y = y)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::ggtitle("SignalTrans + p_0") +
    ggplot2::geom_line(data = data.frame(x = xi,
                              y = getSignalTransPlusOffset(
                                t_prime = xi,
                                tau_1 = tau_1,
                                tau_2 = tau_2,
                                A_trans = A_trans,
                                p_0 = p_0,
                                T_shift = T_shift,
                                signum_TF = signum_TF)),
                       ggplot2::aes(x = x, y = y)) +
    ggplot2::theme_bw()
}
