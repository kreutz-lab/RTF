#' Plot RTF together with experimental data points
#'
#' @description Plot RTF together with experimental data points
#' @return ggplot object showing RTF together with experimental data points
#' @param tau_1 tau_1
#' @param tau_2 tau_2
#' @param A_sus A_sus
#' @param A_trans A_trans
#' @param p_0 p_0
#' @param T_shift T_shift
#' @param signum_TF signum_TF
#' @param y Experimental outcome for time points (corresponds to y column in
#' experimental data frame)
#' @param t Time points
#' @param title Plot title
#' @export plotFit
#' @examples
#' gg <- plotFit(
#'        tau_1 = 1.00, tau_2 = 1.00,
#'        A_sus = 1.05, A_trans = 3.05,
#'        p_0 = -0.28, T_shift = -1, signum_TF = 1,
#'        y = c(0.45, 0.96, 1.13, 1.1, 0.9, 0.76, 0.78),
#'        t = c(0, 0.7, 1.2, 1.55, 2.3, 7.45, 10))

plotFit <- function(
    tau_1, tau_2, A_sus, A_trans, p_0, T_shift, signum_TF, y, t, title = "") {
  xi <- seq(0, max(t), length.out = 1000)

  gg <- ggplot2::ggplot(data.frame(t = t, y = y),
                  ggplot2::aes(x = t, y = y)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_line(data = data.frame(x = xi,
                              y = getTransientFunctionExampleData(
                                t = xi,
                                tau_1 = tau_1,
                                tau_2 = tau_2,
                                A_sus = A_sus,
                                A_trans = A_trans,
                                p_0 = p_0,
                                T_shift = T_shift,
                                signum_TF = signum_TF)),
                       ggplot2::aes(x = x, y = y)) +
    ggplot2::theme_bw()
  
  if (nchar(title) > 0) gg <- gg + ggplot2::ggtitle(title) 
  
  gg
}
