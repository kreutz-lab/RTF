#' Generate example data frame for defined RTF parameters
#'
#' @description Generates an example dataframe with data following an RTF with
#' predefined parameters plus some added noise.
#' @return Data frame with data following an RTF with predefined parameters plus
#' some added noise.
#' @param modus String indicating modus. Either "timeDependent" or 
#' "doseDependent". Default: "timeDependent"
#' @export getExampleDf
#' @examples
#' getExampleDf()

getExampleDf <- function(modus = "timeDependent"){
  if (modus == "timeDependent") {
    t <- seq(0, 17, 0.7)
    
    y <- getTransientFunctionResult(
      t = t,
      rtfPar = c(alpha = 0.004, gamma = 0.0025, A = 1, B = 5, b = 2,
              tau = 4, signum_TF = 1)) 
      
    data.frame(t = t, y = y + stats::rnorm(length(t), 0, 0.04))
  } else if (modus == "doseDependent") {
    t <- seq(0, 17, 0.7)
    doses <- c(2, 4, 6, 9)
    times <- length(doses)

    vec <- c()
    for (dose in doses) {
      
    par <- c(M_alpha = 1,
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
             b = 0.3)
      rtfPar <- getHillResults(d = dose, params = par)
      
      y <- NULL
      y <- getTransientFunctionResult(
        t = t, 
        rtfPar = rtfPar,
        scale = FALSE,
        signum_TF = 1)
      vec <- c(vec, y)
    }
    
    d <- rep(doses, each = length(t))
    t <- rep(t, times)
    df <- data.frame(t = t, y = vec + stats::rnorm(length(t), 0, 0.2), 
                     d = d)
    df <- rbind(c(t = 2, y = NA, d = 2), df)
    df
  }
}
