#' @description Generates an example dataframe with data following an RTF with
#' predefined parameters plus some added noise.
#' @return Dataframe with data following an RTF with predefined parameters plus
#' some added noise.
#' @export getExampleDf
#' @examples
#' getExampleDf()

getExampleDf <- function(){
  t <- seq(0, 10, 0.7)
  y <- getTransientFunctionExampleData(
    tau_1=0.4, tau_2=2, A_sus=0.25, A_trans=0.5, p_0=0.3,
    T_shift=2.5, signum_TF=1, t_prime=t)
  data.frame(t = t, y = y + rnorm(length(t), 0, 0.03))
}
