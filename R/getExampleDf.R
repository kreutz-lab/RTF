#' Generate example data frame for defined RTF parameters
#'
#' @description Generates an example dataframe with data following an RTF with
#' predefined parameters plus some added noise.
#' @return Dataframe with data following an RTF with predefined parameters plus
#' some added noise.
#' @param modus Modus, "RetardedTransientDynamics" or 
#' "DoseDependentRetardedTransientDynamics"
#' @export getExampleDf
#' @examples
#' getExampleDf()

getExampleDf <- function(modus = "RetardedTransientDynamics"){
  if (modus == "RetardedTransientDynamics") {
    t <- seq(0, 17, 0.7)
    
    y <- getTransientFunctionResult(
      t = t,
      par = c(tau_1 = 1, tau_2 = 2, A_sus = 1, A_trans = 2, p_0 = 0.3,
              T_shift = 2, signum_TF = 1),
      modus = "RetardedTransientDynamics") 
      
    data.frame(t = t, y = y + stats::rnorm(length(t), 0, 0.03))
  } else if (modus == "DoseDependentRetardedTransientDynamics") {
    t <- seq(0, 17, 0.7)
    doses <- c(2, 4, 6, 9)
    times <- length(doses)
    
    vec <- c()
    for (dose in doses) {
      y <- NULL
      y <- getTransientFunctionResult(
        t = t, 
        d = dose,
        par = c(
          M_tau1 = 0.1,
          h_tau1 = 1,
          K_tau1 = 30,
          M_tau2 = 2,
          h_tau2 = 7,
          K_tau2 = 55,
          M_Asus = 1,
          h_Asus = 10,
          K_Asus = 0.2, 
          M_Atrans = 6 * dose,
          h_Atrans = 1.5,
          K_Atrans = 8,
          M_Tshift = 4,
          h_Tshift = 10, 
          K_Tshift = 55,
          p_0 = 0.3,
          signum_TF = 1
        ), 
        modus = modus,
        scale = FALSE)
      vec <- c(vec, y)
    }
    
    d <- rep(doses, each = length(t))
    t <- rep(t, times)
    df <- data.frame(t = t, y = vec + stats::rnorm(length(t), 0, 0.05), 
                     d = d)
    df <- rbind(c(t = 2, y= NA, d = 2), df)
    df
  }
}
