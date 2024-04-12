#' Calculate Hill equation
#'
#' @description Calculates Hill equation results for specified values of
#' dose ('d'), maximum value ('M'), Hill coefficient ('h'), and half-maximum 
#' quantity ('K').
#' @return Hill equation result
#' @param d Dose
#' @param M Maximum value
#' @param h Hill coefficient
#' @param K Half-maximum quantity
#' @param reciprocal Boolean indicating if Hill equation is calculated for
#' reciprocal parameter
#' @param gradientNames  names of parameters for which derivatives are 
#' calculated. "M","h","K" should be part of it.
#' If provided, then derivatives are calculated instead of the hill equation 
#' itself.
#' @param minval (Optional) Numeric, defines the lowest value the result can 
#' acquire. Only relevant if reciprocal = TRUE.
#' @export hillEquation
#' @examples
#' hillEquation(d = 20, M = 5, h = 3, K = 4)
#' hillEquation(d = 1:20, M = 5, h = 3, K = 4, 
#'              gradientNames = c("A", "B", "M", "h", "K", "alpha"))

hillEquation <- function(d, M, h, K, reciprocal = FALSE,
                         gradientNames = c(), minval = NULL){
    
    if (length(gradientNames) > 0)
        if (length(setdiff(c("h","M","K"), gradientNames)) > 0)
            warning("gradientNames does not contain all parameters (h,M,K)")
    
    if (reciprocal) {
        if (length(gradientNames) > 0) {
            dresult_dMhp <- matrix(0, nrow = length(d), 
                                   ncol = length(gradientNames))
            colnames(dresult_dMhp) <- gradientNames
            row.names(dresult_dMhp) <- as.character(d)
            dresult_dMhp[,"M"] <- 1 - d^h/(K^h + d^h)
            dresult_dMhp[,"h"] <- -M * ((d^h * log(d))/(K^h + d^h) - 
                                            (d^h * (K^h * log(K) + 
                                                        d^h * log(d))) / 
                                            (K^h + d^h)^2)
            dresult_dMhp[,"K"] <- (K^(h - 1) * M * d^h * h)/(K^h + d^h)^2
        } else {
            result <- M * (1 - ((d^h) / (K^h + d^h)))
            if (!is.null(minval)) {
                result[result < minval] <- minval
                if (length(gradientNames) > 0) 
                    warning("result[result < minval] <- minval cannot be 
                            considered in gradients.")
            }
        }
    } else {
        if (length(gradientNames) > 0) {
            dresult_dMhp <- matrix(0, nrow = length(d), 
                                   ncol = length(gradientNames))
            colnames(dresult_dMhp) <- gradientNames
            row.names(dresult_dMhp) <- as.character(d)
            dresult_dMhp[,"M"] <- d^h / (K^h + d^h)
            dresult_dMhp[,"h"] <- (M * d^h * log(d)) / (K^h + d^h) - 
                (M * d^h * (K^h * log(K) + d^h * log(d))) / (K^h + d^h)^2
            dresult_dMhp[,"K"] <- -(K^(h - 1) * M * d^h * h) / (K^h + d^h)^2
        } else { 
            result <- M * ((d^h) / (K^h + d^h))
        }
    }
    
    if (length(gradientNames) > 0) 
        return(dresult_dMhp)
    else
        return(result)
}