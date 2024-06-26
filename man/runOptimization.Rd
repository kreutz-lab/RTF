% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runOptimization.R
\name{runOptimization}
\alias{runOptimization}
\title{Run optimization}
\usage{
runOptimization(initialGuess.vec.lst, optimObject, objFunct)
}
\arguments{
\item{initialGuess.vec.lst}{List of default initial guess for each model
parameter a defined number of further initial guesses lying between the lower
and upper bound of each model parameter.}

\item{optimObject}{optimObject, which is a list containing input data frame
with time-resolved data ('data'), the vector of initial guesses
('initialGuess.vec'), of lower bounds ('lb.vec'), of upper bounds ('ub.vec'),
vector of fixed parameters ('fixed'), if log10 is applied to bounds
('takeLog10'), the parameters having no negative values in initialGuess.vec,
lb.vec, and ub.vec ('positive.par.names'), modus ('modus'),
log-likelihood function for the parameter optimization ('optimFunction'), if the
sign of the sustained and transient RTF part should be equal ('sameSign'),
list of control parameters passed to stats::optim ('control'), and a list of
values of fitted parameters ('fitted', can be empty).}

\item{objFunct}{Name of the objective function.}
}
\value{
List of with optimization results ('optimResults') and best
optimization result ('bestOptimResult').
}
\description{
Run optimization using stats::optim with method = "L-BFGS-B".
}
\examples{
data <- getSimData()
optimObject.orig <- initializeOptimObject(data, modus = "singleDose")
initialGuess.vec.lst <- getInitialGuessVecLst(
    initialGuess.vec =
        optimObject.orig$initialGuess.vec,
    lb.vec = optimObject.orig$lb.vec,
    ub.vec = optimObject.orig$ub.vec,
    takeLog10 = optimObject.orig$takeLog10,
    nInitialGuesses = 50
)
res <- runOptimization(initialGuess.vec.lst, optimObject.orig, objFunct)
}
