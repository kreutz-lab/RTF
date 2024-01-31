#' Get vectored with data points scaled to lie between 0 and 10
#'
#' @description Scales or reverses scaling for time-dependent variable
#' @return Scaled vector if reverse = FALSE and unscaled timeParam is entered,
#' unscaled vector if reverse = TRUE and already scaled timeParam is entered.
#' @param timeParam Time-dependent variable
#' @param maxVal Maximum of range the time-dependent variable should be scaled to
#' @param reverse Boolean if scaling should be reversed
#' @export scaleTimeParameter
#' @examples
#' t <- c(2, 5, 4, 0.9, 1.2)
#' t_prime <- scaleTimeParameter(timeParam=t, maxVal = max(t))

scaleTimeParameter <- function(timeParam, maxVal = NULL, reverse = FALSE) {
  if (is.null(maxVal)){
    message("Please enter minVal and maxVal as arguments")
  }
 # minVal <- 0
  if (!reverse) {
    timeParam <- timeParam * 10 / maxVal
  } else {
    timeParam <- timeParam * maxVal / 10
  }

  list(timeParam = timeParam,
     maxVal = maxVal)
}
