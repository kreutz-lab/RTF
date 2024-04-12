#' Get named vectors containing the lower bounds, upper bounds, and initial 
#' guesses from data frame of parameters
#'
#' @description Transforms data frame of lower bounds, upper bounds,
#' and initial guesses for each model parameter to list of named vectors.
#' @return List of named vectors of lower bounds ('lb.vec'), upper bounds 
#' ('ub.vec'), and initial guesses ('initialGuess.vec') for each model 
#' parameter.
#' @param df Data frame containing columns named 'lb.vec' (lower bounds),
#' 'ub.vec' (upper bounds), 'initialGuess.vec' (initial guesses), and of which
#' the row names are the names of the model parameters.
#' @export getNamedVectorsFromDf
#' @examples
#' len <- 7
#' lb.vec <- rnorm(len)
#' ub.vec <- lb.vec + runif(len, 0, 2)
#' initialGuess.vec <- runif(len, lb.vec, ub.vec)
#' df <- data.frame(
#'        lb.vec = lb.vec, ub.vec = ub.vec, initialGuess.vec = initialGuess.vec)
#' row.names(df) <- paste0("param", 1:len)
#' lst <- getNamedVectorsFromDf(df)
#' lst

getNamedVectorsFromDf <- function(df) {
    lb.vec <- df$lb.vec
    ub.vec <- df$ub.vec
    initialGuess.vec <- df$initialGuess.vec
    names(lb.vec) <- names(ub.vec) <- names(initialGuess.vec) <- row.names(df)
    list(lb.vec = lb.vec, ub.vec = ub.vec, initialGuess.vec = initialGuess.vec)
}
