#' Get names of parameters having no negative values
#'
#' @description Returns the names of parameters having no negative values in
#' initialGuess.vec, lb.vec, and ub.vec.
#' @return Vector of parameters having no negative values in
#' initialGuess.vec, lb.vec, and ub.vec
#' @param ub.vec Named vector of upper bound for each model parameter
#' @param lb.vec Named vector of lower bound for each model parameter
#' @param initialGuess.vec Named vector of initial guesses for each model 
#' parameter
#' @export getPositiveParNames
#' @examples
#' len <- 20
#' lb.vec <- rnorm(len)
#' ub.vec <- lb.vec + runif(len, 0, 2)
#' initialGuess.vec <- runif(len, lb.vec, ub.vec)
#' names(lb.vec) <- names(ub.vec) <- names(initialGuess.vec) <-
#'                                                       paste0("param", 1:len)
#' getPositiveParNames(lb.vec, ub.vec, initialGuess.vec)

getPositiveParNames <- function(lb.vec, ub.vec, initialGuess.vec) {
    boundsDefault.df <- data.frame(
        lb.vec, ub.vec, initialGuess.vec)
    par.names <- row.names(boundsDefault.df)
    positive.par.names <- par.names[
        which(apply(boundsDefault.df, 1, function(x) !any(x <= 0)))]
    positive.par.names
}
