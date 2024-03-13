#' Take Log10(x) or 10^(x) for a vector x
#'
#' @description Take Log10(x) or 10^(x) for a vector x
#' @return Named vector where the parameters, which are 
#' @param x Named vector with parameter values
#' @param takeLog10 Named vector of booleans indicating if log10 is/should be 
#' taken of the model parameters.
#' @param reverse Boolean indicating if 10^ should be taken instead of log10() 
#' (Default: FALSE).
#' @param calcGradient Boolean indicating if gradient should be calculated
#' (Default: FALSE)
#' @export applyLog10ForTakeLog10
#' @examples
#' data <- getSimData()
#' optimObject.orig <- initializeOptimObject(data,
#'                                          modus = 'timeDependent')
#' initialGuess.vec <- optimObject.orig[["initialGuess.vec"]]
#' lb.vec <- optimObject.orig[["lb.vec"]]
#' takeLog10 <- optimObject.orig[["takeLog10"]]
#' applyLog10ForTakeLog10(lb.vec, takeLog10) 
#' 
#' 
#' x <- 1:3
#' names(x) <- c("A","B","C")
#' takeLog10 <- c(TRUE, TRUE, FALSE)
#' names(takeLog10) <- names(x)
#' applyLog10ForTakeLog10(x,takeLog10 = takeLog10)
#' applyLog10ForTakeLog10(x,takeLog10 = takeLog10, calcGradient = TRUE)

applyLog10ForTakeLog10 <- function(x = c(), takeLog10 = c(), reverse = FALSE, 
                                   calcGradient = FALSE) {
  
  y <- x
  dy_dx <- matrix(0, nrow = length(y), ncol = length(x))
  diag(dy_dx) <- 1
  diagAll <- diag(dy_dx)
  
  bol <- names(x) %in% names(which(takeLog10))
  
  if (reverse) {
    y[bol] <- 10^x[bol]
    if (calcGradient) diagAll[bol] <- 10^x[bol] * log(10)
  } else {
    y[bol] <- log10(x[bol])
    if (calcGradient) diagAll[bol] <- 1 / (x[bol] * log(10))
  }
  
  if (calcGradient) {
    diag(dy_dx) <- diagAll
    colnames(dy_dx) <- names(x)
    dy_dx
  } else {
    names(y) <- names(x)
    y
  }
}
