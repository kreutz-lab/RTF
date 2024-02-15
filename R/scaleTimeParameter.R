#' Get vectored with data points scaled to lie between 0 and 10
#'
#' @description Scales or reverses scaling for time-dependent variable
#' @return Scaled vector if reverse = FALSE and unscaled timeParam is entered,
#' unscaled vector if reverse = TRUE and already scaled timeParam is entered.
#' @param timeParam Time-dependent variable
#' @param maxVal Maximum of range the time-dependent variable should be scaled 
#' to
#' @param reverse Boolean if scaling should be reversed
#' @param calcGradient Boolean indicating if gradient should be calculated
#' @export scaleTimeParameter
#' @examples
#' t <- c(2, 5, 4, 0.9, 1.2)
#' t_prime <- scaleTimeParameter(timeParam=t, maxVal = max(t))
#' scaleTimeParameter(timeParam=c(tau=1), maxVal = max(t),gradientNames = c("tau","alpha"))

scaleTimeParameter <- function(timeParam, maxVal = NULL, reverse = FALSE,
                               gradientNames = c()) {
  if (is.null(maxVal)) {
    message("Please enter maxVal as arguments")
  }
  # if (!reverse) {
    if (length(gradientNames)>0) {
      dtimeParam_dgradNames <- matrix(0,nrow=length(timeParam),ncol=length(gradientNames))
      colnames(dtimeParam_dgradNames) <- gradientNames
      rownames(dtimeParam_dgradNames) <- names(timeParam)
      
      dtimeParam_dgradNames[,names(timeParam)] = maxVal
    } else {
      timeParam <- timeParam  * maxVal
    }
  # } else {
  #   if (length(gradientNames)>0) {
  #     dtimeParam_dgradNames <- matrix(0,nrow=length(timeParam),ncol=length(gradientNames))
  #     colnames(dtimeParam_dgradNames) <- gradientNames
  #     rownames(dtimeParam_dgradNames) <- names(timeParam)
  #     
  #     dtimeParam_dgradNames[,names(timeParam)] <- maxVal 
  #   } else {
  #     timeParam <- timeParam * maxVal
  #   }
  # }
  
  if (length(gradientNames)>0) {
    list(timeParam=dtimeParam_dgradNames, maxVal=maxVal)
  } else {
    list(timeParam = timeParam,
         maxVal = maxVal)
  }
}
