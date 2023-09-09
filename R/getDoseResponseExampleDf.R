#' Generate example data frame for defined dose-response RTF parameters
#'
#' @description Generates an example dataframe with data following an RTF with
#' predefined parameters plus some added noise, for different doses d.
#' @return Dataframe with data following an RTF for different doses d
#'  with predefined parameters plus some added noise.
#' @export getDoseResponseExampleDf
#' @examples
#' getDoseResponseExampleDf()

getDoseResponseExampleDf <- function(){
  t <- seq(3, 17, 0.7)
  y2 <- getTransientFunctionExampleData(
    tau_1=1, tau_2=2, A_sus=1, A_trans=2, p_0=0.3,
    T_shift=2, signum_TF=1, t=t)
  y4 <- getTransientFunctionExampleData(
    tau_1=1, tau_2=2, A_sus=1, A_trans=4, p_0=0.3,
    T_shift=2, signum_TF=1, t=t)
  y6 <- getTransientFunctionExampleData(
    tau_1=1, tau_2=2, A_sus=1, A_trans=6, p_0=0.3,
    T_shift=2, signum_TF=1, t=t)
  y9 <- getTransientFunctionExampleData(
    tau_1=1, tau_2=2, A_sus=1, A_trans=9, p_0=0.3,
    T_shift=2, signum_TF=1, t=t)
  d <- rep(c(2, 4, 6, 9), each = length(t))
  t <- rep(t, 4)
  data.frame(t = t, y = c(y2, y4, y6, y9) + stats::rnorm(length(t), 0, 0.03), 
             d = d)
}
