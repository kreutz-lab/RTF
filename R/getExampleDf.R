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
      par = c(alpha = 0.004, gamma = 0.0025, A = 1, B = 5, b = 2,
              tau = 4, signum_TF = 1),
      modus = "RetardedTransientDynamics") 
      
    data.frame(t = t, y = y + stats::rnorm(length(t), 0, 0.04))
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
          M_alpha = 1,
          h_alpha = 0.1,
          K_alpha = 0.03,
          M_gamma = 0.5,
          h_gamma = 0.2,
          K_gamma = 0.01,
          M_A = 1,
          h_A = 10,
          K_A = 0.2, 
          M_B = 6 * dose,
          h_B = 1.5,
          K_B = 8,
          M_tau = 4,
          h_tau = 6, 
          K_tau = 20,
          b = 0.3,
          signum_TF = 1
        ), 
        modus = modus,
        scale = FALSE)
      vec <- c(vec, y)
    }
    
    d <- rep(doses, each = length(t))
    t <- rep(t, times)
    df <- data.frame(t = t, y = vec + stats::rnorm(length(t), 0, 0.2), 
                     d = d)
    df <- rbind(c(t = 2, y= NA, d = 2), df)
    df
  }
}
