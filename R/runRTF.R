#' Run RTF
#'
#' @description Run RTF
#' @return List of the final RTF model (finalModel), the optimized parameters
#' (finalParams), the plot of the final model (finalPlot), as well as the
#' intermediate results (intermediateResults).
#' @param data Data frame containing columns named 't' (time) and
#' 'y' (quantitative value)
#' @param modus String indicating if modus 'RetardedTransientDynamics' or
#' 'ImmediateResponseFunction' should be used
#' @param optimFunction String indicating the optimization function which 
#' should be used (Default: "chiSquare")
#' @param control List of control arguments passed to the function stats::optim 
#' (Default: list(trace = 1, maxit = 1000, factr = 1.0e-20))
#' @export runRTF
#' @examples
#' modus <- "RetardedTransientDynamics"
#' data <- getExampleDf()
#' plot(data)
#' res <- runRTF(data, modus = modus)

runRTF <- function(data, modus = "RetardedTransientDynamics", 
                   optimFunction = "chiSquare", 
                   control = list(trace = 1, maxit = 1000,
                                  factr = 1.0e-20)) {
  optimObject.orig <- initializeOptimObject(data, modus = modus, 
                                            optimFunction = optimFunction)
  res.all.plusMinus <- getFittingResult(optimObject.orig,
                                        titlePrefixPrefix = "fullModel_")
  res <- selectBest(res.all.plusMinus)

  #  MODEL REDUCTION
  # 1. Testing whether there is time retardation,
  # i.e., if T_shift parameter is significantly different from the lower bound.
  #  If not significant, T_shift is set to the lower bound which is
  # T_shift = −2 by default.
  optimObjectTmp <- optimObject.orig
  optimObjectTmp$positive.par.names <-
    setdiff(optimObjectTmp$positive.par.names, "T_shift")  # because lb.vec[["T_shift"]] corresponds to -2
  optimObjectTmp$fixed[["T_shift"]] <- optimObject.orig$lb.vec[["T_shift"]]
  res.T_shiftLB.plusMinus <- getFittingResult(
    optimObjectTmp,
    titlePrefixPrefix = "TshiftFixed_")
  res.T_shiftLB <- selectBest(res.T_shiftLB.plusMinus)
  res <- selectSmallerModelIfDiffIsSmall(res, res.T_shiftLB)

  # 2. Testing whether the model is in agreement with a constant.
  # If not significant, we set A_sus = A_trans = 0.
  optimObjectTmp2 <- res
  optimObjectTmp2$positive.par.names <-
    setdiff(optimObjectTmp2$positive.par.names, c("A_sus", "A_trans"))  # because lb.vec[["T_shift"]] corresponds to -2
  optimObjectTmp2$fixed[["A_sus"]] <- optimObjectTmp2$fixed[["A_trans"]] <- 0
  res.constant.plusMinus <- getFittingResult(
    optimObjectTmp2, titlePrefixPrefix = "Constant_")
  res.constant <- selectBest(res.constant.plusMinus)
  res <- selectSmallerModelIfDiffIsSmall(res, res.constant)

  # 3. Testing whether the offset p0 is significantly different from zero.
  # If not significant, we set p_0 = 0.
  optimObjectTmp3 <- res
  optimObjectTmp3$positive.par.names <-
    setdiff(optimObjectTmp3$positive.par.names, "p_0")  # because lb.vec[["T_shift"]] corresponds to -2
  optimObjectTmp3$A_sus <- optimObjectTmp3$p_0  <- 0
  res.p_0Zero.plusMinus <- getFittingResult(
    optimObjectTmp3, titlePrefixPrefix = "p0Zero_")
  res.p_0Zero <- selectBest(res.p_0Zero.plusMinus)
  res <- selectSmallerModelIfDiffIsSmall(res, res.p_0Zero)

  finalModel <- res
  finalParams <- res$fitted

  print("The parameters of the best fit are:")
  print(paste(names(finalParams), round(finalParams, 4),
              sep = ": ", collapse = ", "))
  
  return(list(finalModel = res,
              finalParams = res$fitted,
              intermediateResults = list(fullModel = res.all.plusMinus,
                                         TshiftFixed = res.T_shiftLB.plusMinus,
                                         Constant = res.constant.plusMinus,
                                         p0Zero = res.p_0Zero.plusMinus)))

}
