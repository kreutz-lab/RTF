#' @description Adds column 't_prime' to input data frame, which scaleTime is
#' set to TRUE corresponds to 't' scaled to interval [0, 10],
#' otherwise 't_prime' corresponds to 't'
#' @return Data frame which contains an additional column 't_prime'
#' @param data Data frame which contains time resolved data,
#' with columns 't' (time) and 'y' (quantitative value)
#' @param scaleTime Boolean value indicating if time should be scaled should be
#' scaled such that they lie in the interval [0, 10]
#' @export scaleTimeCol
#' @examples
#' data <- getExampleDf()
#' data.scaled <- scaleTimeCol(data, scaleTime = TRUE)

scaleTimeCol <- function(data, scaleTime = TRUE) {
  t <- data$t
  y <- data$y

  if (scaleTime){
    t_range <- max(t)-min(t)
    t_prime <- 10*t/t_range
  } else {
    t_prime <- data$t
  }
  data$t_prime <- t_prime
  data
}
