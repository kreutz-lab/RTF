#' Generate example data frame for defined RTF parameters
#'
#' @description Generates an example dataframe with data following an RTF with
#' predefined parameters plus some added noise.
#' @return Dataframe with data following an RTF with predefined parameters plus
#' some added noise.
#' @export getExampleDf
#' @examples
#' getExampleDf()

getExampleDf <- function(){
  t <- seq(3, 17, 0.7)
  y <- getTransientFunctionExampleData(
    tau_1=1, tau_2=2, A_sus=1, A_trans=1.5, p_0=0.3,
    T_shift=2, signum_TF=1, t=t)
  data.frame(t = t, y = y + stats::rnorm(length(t), 0, 0.03))
}
