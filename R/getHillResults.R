#' Generate data frame for A, B, alphaInv, gammaInv, and tau for
#'  one or multiple defined doses
#'
#' @description Generate data frame for A, B, alphaInv, gammaInv, and 
#' tau for one or multiple defined doses
#' @return Data frame with the caclulated A, B, alphaInv, gammaInv, and 
#' tau, where each row corresponds to the dose procided as a single value or
#' a vector.
#' @param d Dose given as single value or a vector
#' @param params Named vector containing the parameters 'M_alphaInv', 'h_alphaInv', 'K_alphaInv', 
#' 'M_gammaInv', 'h_gammaInv', 'K_gammaInv', 'M_A', 'h_A', 'K_A', 
#' 'M_B', 'h_B', 'K_B', 'M_tau', 'h_tau', 'K_tau'
#' (but can also contain additional ones)
#' @export getHillResults
#' @examples
#' df <- getHillResults(d = c(2, 6),
#'                      params = c(M_alphaInv = 1, h_alphaInv = 2, K_alphaInv = 2,
#'                                 M_gammaInv = 1, h_gammaInv = 3, K_gammaInv = 2,
#'                                 M_A = 4, h_A = 2, K_A = 1,
#'                                 M_B = 2, h_B = 3, K_B = 1,
#'                                 M_tau = 2, h_tau = 3, K_tau = 2))

getHillResults <- function(d = NULL, 
                           params = c()) {
  
  for (v in 1:length(params)) assign(names(params)[v], params[[v]])
  
  # exists.m <- function(x) {
  #   all(sapply(x, exists))
  # }
  # 
  # if (!exists.m(c("d", 
  #                 "M_alphaInv", "h_alphaInv", "K_alphaInv", 
  #                 "M_gammaInv", "h_gammaInv", "K_gammaInv", 
  #                 "M_A", "h_A", "K_A", 
  #                 "M_B", "h_B", "K_B", 
  #                 "M_tau", "h_tau", "K_tau"))) {
  #   stop(paste0("Please provide values for all parameters:","
  #        'd', 'M_alphaInv', 'h_alphaInv', 'K_alphaInv', 
  #       'M_gammaInv', 'h_gammaInv', 'K_gammaInv', 
  #       'M_A', 'h_A', 'K_A', 
  #       'M_B', 'h_B', 'K_B', 
  #       'M_tau', 'h_tau', 'K_tau' to function getHillResults"))
  # }
  # 
  # if (!exists("d")) warning("d missing in function getHillResults")
  # if (!exists("M_alphaInv")) warning("M_alphaInv missing in function getHillResults")  
  # if (!exists("h_alphaInv")) warning("h_alphaInv missing in function getHillResults")  
  # if (!exists("K_alphaInv")) warning("K_alphaInv missing in function getHillResults")  

  for (el in c("d", 
               "M_alphaInv", "h_alphaInv", "K_alphaInv", 
               "M_gammaInv", "h_gammaInv", "K_gammaInv", 
               "M_A", "h_A", "K_A", 
               "M_B", "h_B", "K_B", 
               "M_tau", "h_tau", "K_tau")) {
    if (!exists(el)) warning(paste0(el, " missing in function getHillResults"))
  }
  
  A <- hillEquation(d = d, M = M_A, h = h_A, K = K_A)
  B <- hillEquation(d = d, M = M_B, h = h_B, K = K_B)
  alphaInv <- hillEquationReciprocal(d = d, M = M_alphaInv, 
                                  h = h_alphaInv, K = K_alphaInv, minval = 1e-6)
  gammaInv <- hillEquationReciprocal(d = d, M = M_gammaInv, 
                                  h = h_gammaInv, K = K_gammaInv, minval = 1e-6)
  tau <- hillEquationReciprocal(d = d, M = M_tau, 
                                    h = h_tau, K = K_tau)
  
  df <- data.frame(d = d,
                   A = A,
                   B = B,
                   alphaInv = alphaInv,
                   gammaInv = gammaInv,
                   tau = tau)
  df
}
