#' Generate data frame for A_sus, A_trans, tau_1, tau_2, and T_shift for
#'  one or multiple defined doses
#'
#' @description Generate data frame for A_sus, A_trans, tau_1, tau_2, and 
#' T_shift for one or multiple defined doses
#' @return Data frame with the caclulated A_sus, A_trans, tau_1, tau_2, and 
#' T_shift, where each row corresponds to the dose procided as a single value or
#' a vector.
#' @param d Dose given as single value or a vector
#' @param params Named vector containing the parameters 'M_tau1', 'h_tau1', 'K_tau1', 
#' 'M_tau2', 'h_tau2', 'K_tau2', 'M_Asus', 'h_Asus', 'K_Asus', 
#' 'M_Atrans', 'h_Atrans', 'K_Atrans', 'M_Tshift', 'h_Tshift', 'K_Tshift'
#' (but can also contain additional ones)
#' @export getHillResults
#' @examples
#' df <- getHillResults(d = c(2, 6),
#'                      params = c(M_tau1 = 1, h_tau1 = 2, K_tau1 = 2,
#'                                 M_tau2 = 1, h_tau2 = 3, K_tau2 = 2,
#'                                 M_Asus = 4, h_Asus = 2, K_Asus = 1,
#'                                 M_Atrans = 2, h_Atrans = 3, K_Atrans = 1,
#'                                 M_Tshift = 2, h_Tshift = 3, K_Tshift = 2))

getHillResults <- function(d = NULL, 
                           params = c()) {
  
  for (v in 1:length(params)) assign(names(params)[v], params[[v]])
  
  # exists.m <- function(x) {
  #   all(sapply(x, exists))
  # }
  # 
  # if (!exists.m(c("d", 
  #                 "M_tau1", "h_tau1", "K_tau1", 
  #                 "M_tau2", "h_tau2", "K_tau2", 
  #                 "M_Asus", "h_Asus", "K_Asus", 
  #                 "M_Atrans", "h_Atrans", "K_Atrans", 
  #                 "M_Tshift", "h_Tshift", "K_Tshift"))) {
  #   stop(paste0("Please provide values for all parameters:","
  #        'd', 'M_tau1', 'h_tau1', 'K_tau1', 
  #       'M_tau2', 'h_tau2', 'K_tau2', 
  #       'M_Asus', 'h_Asus', 'K_Asus', 
  #       'M_Atrans', 'h_Atrans', 'K_Atrans', 
  #       'M_Tshift', 'h_Tshift', 'K_Tshift' to function getHillResults"))
  # }
  # 
  # if (!exists("d")) warning("d missing in function getHillResults")
  # if (!exists("M_tau1")) warning("M_tau1 missing in function getHillResults")  
  # if (!exists("h_tau1")) warning("h_tau1 missing in function getHillResults")  
  # if (!exists("K_tau1")) warning("K_tau1 missing in function getHillResults")  

  for (el in c("d", 
               "M_tau1", "h_tau1", "K_tau1", 
               "M_tau2", "h_tau2", "K_tau2", 
               "M_Asus", "h_Asus", "K_Asus", 
               "M_Atrans", "h_Atrans", "K_Atrans", 
               "M_Tshift", "h_Tshift", "K_Tshift")) {
    if (!exists(el)) warning(paste0(el, " missing in function getHillResults"))
  }
  
  A_sus <- hillEquation(d = d, M = M_Asus, h = h_Asus, K = K_Asus)
  A_trans <- hillEquation(d = d, M = M_Atrans, h = h_Atrans, K = K_Atrans)
  tau_1 <- hillEquationReciprocal(d = d, M = M_tau1, 
                                  h = h_tau1, K = K_tau1, minval = 1e-6)
  tau_2 <- hillEquationReciprocal(d = d, M = M_tau2, 
                                  h = h_tau2, K = K_tau2, minval = 1e-6)
  T_shift <- hillEquationReciprocal(d = d, M = M_Tshift, 
                                    h = h_Tshift, K = K_Tshift)
  
  df <- data.frame(d = d,
                   A_sus = A_sus,
                   A_trans = A_trans,
                   tau_1 = tau_1,
                   tau_2 = tau_2,
                   T_shift = T_shift)
  df
}
