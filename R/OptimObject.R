#' @title Class: OptimObject
#' @description Description for OptimObject
#' @slot data Data frame with time resolved data, containing at least the
#' numeric columns 'y' ( measured outcome) and 't' (time)
#' @slot initialGuess.vec Named vector of initial guesses
#' @slot lb.vec Named vector of lower bounds
#' @slot ub.vec Named vector of upper bounds
#' @slot fixed Named vector of fixed parameters
#' @slot takeLog10 Boolean if log10 is applied to bounds
#' @slot positive.par.names Named vector of the parameters having only values
#' above 0 in initialGuess.vec, lb.vec, and ub.vec
#' @slot modus 'RetardedTransientDynamics' or 'ImmediateResponseFunction'
#' @slot fitted List of values of fitted parameters
#' @examples
#' OptimObject1 <- new("OptimObject",
#'   data = data.frame(y=c(0.5, 4, 73), t = c(2, 5, 7)),
#'   initialGuess.vec = c(par1 = 2, par2 = 54),
#'   lb.vec = c(par1 = 0, par2 = 0),
#'   ub.vec = c(par1 = 50, par2 = 50),
#'   fixed = c(par1 = 4),
#'   takeLog10 = TRUE,
#'   positive.par.names = c("par1", "par2"),
#'   modus = 'RetardedTransientDynamics',
#'   fitted = c(par1 = 4, par2 = 2)
#'  )
methods::setClass(
    Class = "OptimObject",
    slots = c(data = "data.frame",
              initialGuess.vec = "numeric",
              lb.vec = "numeric",
              ub.vec = "numeric",
              fixed = "numeric",
              takeLog10 = "logical",
              positive.par.names = "character",
              modus = "character",
              fitted = "numeric"
              ),
    prototype = list(
      data =  data.frame(),
      initialGuess.vec = structure(numeric(), names = character()),
      lb.vec = structure(numeric(), names = character()),
      ub.vec = structure(numeric(), names = character()),
      fixed = structure(numeric(), names = character()),
      takeLog10 = structure(logical(), names = character()),
      positive.par.names = character(),
      modus = character(),
      fitted = numeric()
    )
)
