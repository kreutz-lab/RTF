#' Log10-transform initial guesses, lower bounds, and upper bounds ('ub.vec')
#' of OptimObject
#'
#' @description Replace the parameter values of vector of initial guesses
#' ('initialGuess.vec'), of lower bounds ('lb.vec'),
#' of upper bounds ('ub.vec') of OptimObject with the log10 transformed values.
#' Only those parameter values are replaced which have values bigger than 0 in
#' all three vectors, which needs to be indicated by the respective parameter
#' name being included in the vector 'positive.par.names' of the OptimObject.
#' @return OptimObject with values of vector of initial guesses
#' ('initialGuess.vec'), of lower bounds ('lb.vec'),
#' of upper bounds ('ub.vec') of OptimObject being log10 transformed.
#' @param optimObject OptimObject,  which is a list containing input data frame
#' with time-resolved data ('data'), the vector of initial guesses
#' ('initialGuess.vec'), of lower bounds ('lb.vec'), of upper bounds ('ub.vec'),
#' vector of fixed parameters ('fixed'), if log10 is applied to bounds
#' ('takeLog10'), the parameters having no negative values in initialGuess.vec,
#' lb.vec, and ub.vec ('positive.par.names'), modus ('modus'),
#' log-likelihood function for the parameter optimization ('optimFunction'), if the
#' sign of the sustained and transient RTF part should be equal ('sameSign'),
#' list of control parameters passed to stats::optim ('control'), and a list of
#' values of fitted parameters ('fitted', can be empty).
#' @export takeLog10OfBounds
#' @examples
#' data <- getSimData()
#' optimObject <- initializeOptimObject(data,
#'     modus = "singleDose",
#'     takeLog10 = FALSE
#' )
#' positive.par.names <- getPositiveParNames(
#'     lb.vec = optimObject$lb.vec,
#'     ub.vec = optimObject$ub.vec,
#'     initialGuess.vec =
#'         optimObject$initialGuess.vec
#' )
#' positive.par.names <- setdiff(positive.par.names, "sigma")
#' optimObject$positive.par.names <- positive.par.names
#' optimObject.log10 <- takeLog10OfBounds(optimObject)
takeLog10OfBounds <- function(optimObject) {
    boundsDefault.df <- data.frame(
        lb.vec = optimObject$lb.vec,
        ub.vec = optimObject$ub.vec,
        initialGuess.vec =
            optimObject$initialGuess.vec
    )
    positive.par.names <- optimObject$positive.par.names
    optimObject[["takeLog10"]][positive.par.names] <- TRUE
    boundsDefault.df[positive.par.names, ] <-
        log10(boundsDefault.df[positive.par.names, ])
    vecs <- getNamedVectorsFromDf(boundsDefault.df)
    optimObject$lb.vec <- vecs$lb.vec
    optimObject$ub.vec <- vecs$ub.vec
    optimObject$initialGuess.vec <- vecs$initialGuess.vec
    optimObject
}
