#' Multiply time-dependent variables by a factor.
#'
#' @description Multiply time-dependent variables by a factor.
#' @return List with vector of transformed time-dependent variables (if 
#' gradientNames are not provided) or matrix of derivatives (if gradientNames 
#' are provided)('timeParam') and factorVal.
#' @param timeParam Vector of time-dependent variables.
#' @param factorVal Numeric value representing factor value the time-dependent 
#' variables should be multiplied with.
#' @param gradientNames Names of parameters for which derivatives are 
#' calculated "M","h","K" should be part of it.
#' @export multiplyTimeParamsByFactor
#' @examples
#' t <- c(2, 5, 4, 0.9, 1.2)
#' 
#' # Without gradient
#' multiplyTimeParamsByFactor(timeParam = t, factorVal = max(t))
#' 
#' # With gradient
#' multiplyTimeParamsByFactor(
#'   timeParam = c(tau = 1), factorVal = max(t), 
#'   gradientNames = c("tau", "alpha"))

multiplyTimeParamsByFactor <- function(timeParam, factorVal = NULL, 
                                       gradientNames = c()) {
    if (is.null(factorVal)) {
        message("Please enter factorVal as arguments")
    }
    
    if (length(gradientNames) > 0) {
        dtimeParam_dgradNames <- matrix(0, 
                                        nrow = length(timeParam), 
                                        ncol = length(gradientNames))
        colnames(dtimeParam_dgradNames) <- gradientNames
        rownames(dtimeParam_dgradNames) <- names(timeParam)
        dtimeParam_dgradNames[, names(timeParam)] <- factorVal
    } else {
        timeParam <- timeParam  * factorVal
    }
    
    if (length(gradientNames) > 0) {
        list(timeParam = dtimeParam_dgradNames, factorVal = factorVal)
    } else {
        list(timeParam = timeParam, factorVal = factorVal)
    }
}
