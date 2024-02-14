#' Generate data frame for A, B, alpha, gamma, and tau for
#'  one or multiple defined doses
#'
#' @description Generate data frame for A, B, alpha, gamma, and 
#' tau for one or multiple defined doses
#' @return Data frame with the caclulated A, B, alpha, gamma, and 
#' tau, where each row corresponds to the dose procided as a single value or
#' a vector.
#' @param d Dose given as single value or a vector
#' @param params Named vector containing the parameters 'M_alpha', 'h_alpha', 'K_alpha', 
#' 'M_gamma', 'h_gamma', 'K_gamma', 'M_A', 'h_A', 'K_A', 
#' 'M_B', 'h_B', 'K_B', 'M_tau', 'h_tau', 'K_tau'
#' (but can also contain additional ones)
#' @export getHillResults
#' @examples
#' df <- getHillResults(d = c(2, 6),
#'                      params = c(M_alpha = 1, h_alpha = 2, K_alpha = 2,
#'                                 M_gamma = 1, h_gamma = 3, K_gamma = 2,
#'                                 M_A = 4, h_A = 2, K_A = 1,
#'                                 M_B = 2, h_B = 3, K_B = 1,
#'                                 M_tau = 2, h_tau = 3, K_tau = 2))

getHillResults <- function(d = NULL, 
                           params = c(),calcGradient = F) {
  
  for (v in 1:length(params)) assign(names(params)[v], params[[v]])
  
  # exists.m <- function(x) {
  #   all(sapply(x, exists))
  # }
  # 
  # if (!exists.m(c("d", 
  #                 "M_alpha", "h_alpha", "K_alpha", 
  #                 "M_gamma", "h_gamma", "K_gamma", 
  #                 "M_A", "h_A", "K_A", 
  #                 "M_B", "h_B", "K_B", 
  #                 "M_tau", "h_tau", "K_tau"))) {
  #   stop(paste0("Please provide values for all parameters:","
  #        'd', 'M_alpha', 'h_alpha', 'K_alpha', 
  #       'M_gamma', 'h_gamma', 'K_gamma', 
  #       'M_A', 'h_A', 'K_A', 
  #       'M_B', 'h_B', 'K_B', 
  #       'M_tau', 'h_tau', 'K_tau' to function getHillResults"))
  # }
  # 
  # if (!exists("d")) warning("d missing in function getHillResults")
  # if (!exists("M_alpha")) warning("M_alpha missing in function getHillResults")  
  # if (!exists("h_alpha")) warning("h_alpha missing in function getHillResults")  
  # if (!exists("K_alpha")) warning("K_alpha missing in function getHillResults")  

  for (el in c("d", 
               "M_alpha", "h_alpha", "K_alpha", 
               "M_gamma", "h_gamma", "K_gamma", 
               "M_A", "h_A", "K_A", 
               "M_B", "h_B", "K_B", 
               "M_tau", "h_tau", "K_tau")) {
    if (!exists(el)) warning(paste0(el, " missing in function getHillResults"))
  }
  
  A <- hillEquation(d = d, M = M_A, h = h_A, K = K_A)
  B <- hillEquation(d = d, M = M_B, h = h_B, K = K_B)
  alpha <- hillEquation(d = d, M = M_alpha, h = h_alpha, K = K_alpha)
  gamma <- hillEquation(d = d, M = M_gamma, h = h_gamma, K = K_gamma)
  tau <- hillEquationReciprocal(d = d, M = M_tau, 
                                    h = h_tau, K = K_tau)
  
  df <- data.frame(d = d,
                   A = A,
                   B = B,
                   alpha = alpha,
                   gamma = gamma,
                   tau = tau)
  df
}
