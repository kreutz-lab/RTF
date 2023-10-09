#' Run RTF
#'
#' @description Run RTF
#' @return List of the final RTF model (finalModel), the optimized parameters
#' (finalParams), the plot of the final model (finalPlot), as well as the
#' intermediate results (intermediateResults).
#' @param data Data frame containing columns named 't' (time) and
#' 'y' (quantitative value)
#' @param modus String indicating if modus 'RetardedTransientDynamics' ('D'), 
#' 'ImmediateResponseFunction' ('I') or
#' 'DoseDependentRetardedTransientDynamics' ('DD') should be used. 
#' If no modus is provided default setting are  
#' 'DoseDependentRetardedTransientDynamics' if column with name 'd' is present 
#' and else 'RetardedTransientDynamics'.
#' @param optimFunction String indicating the optimization function which 
#' should be used (Default: "chiSquare")
#' @param control List of control arguments passed to the function stats::optim 
#' (Default: list(trace = 1, maxit = 1000, factr = 1.0e-20))
#' @export runRTF
#' @examples
#' modus <- "RetardedTransientDynamics"
#' data <- getExampleDf()
#' plotData(data)
#' res <- runRTF(data, modus = modus)

runRTF <- function(data, modus = NULL, 
                   optimFunction = "chiSquare", 
                   control = list(trace = 1, maxit = 1000,
                                  factr = 1.0e-20)) {
  if (is.null(modus)) {
    if (("d" %in% names(data))) {
      modus <- 'DoseDependentRetardedTransientDynamics'
    } else {
      modus <- 'RetardedTransientDynamics'
    }
  } else {
    if (modus == 'D') modus <- 'RetardedTransientDynamics'
    if (modus == 'I') modus <- 'ImmediateResponseFunction'
    if (modus == 'DD') modus <- 'DoseDependentRetardedTransientDynamics'
  }
  
  optimObject.orig <- initializeOptimObject(data, modus = modus, 
                                            optimFunction = optimFunction)
  res.all.plusMinus <- getFittingResult(optimObject.orig)
  res <- selectBest(res.all.plusMinus)
  
  optimParamsFullModel <- res[["bestOptimResult"]][["par"]]
  intermediateResults <- list(fullModel = res.all.plusMinus,
                              TshiftFixed = NULL,
                              Constant = NULL,
                              p0Zero = NULL)
  
  if (modus != "DoseDependentRetardedTransientDynamics") {
    #  MODEL REDUCTION
    # 1. Testing whether there is time retardation,
    # i.e., if T_shift parameter is significantly different from the lower bound.
    #  If not significant, T_shift is set to the lower bound which is
    # T_shift = −2 by default.
    optimObjectTmp <- optimObject.orig
    optimObjectTmp$positive.par.names <-
      setdiff(optimObjectTmp$positive.par.names, "T_shift")  # because lb.vec[["T_shift"]] corresponds to -2
    optimObjectTmp$fixed[["T_shift"]] <- optimObject.orig$lb.vec[["T_shift"]]
    res.T_shiftLB.plusMinus <- getFittingResult(optimObjectTmp)
    res.T_shiftLB <- selectBest(res.T_shiftLB.plusMinus)
    res <- selectSmallerModelIfDiffIsSmall(res, res.T_shiftLB)
    
    # 2. Testing whether the model is in agreement with a constant.
    # If not significant, we set A_sus = A_trans = 0.
    optimObjectTmp2 <- res
    optimObjectTmp2$positive.par.names <-
      setdiff(optimObjectTmp2$positive.par.names, c("A_sus", "A_trans"))  
    optimObjectTmp2$fixed[["A_sus"]] <- optimObjectTmp2$fixed[["A_trans"]] <- 0
    res.constant.plusMinus <- getFittingResult(optimObjectTmp2)
    res.constant <- selectBest(res.constant.plusMinus)
    res <- selectSmallerModelIfDiffIsSmall(res, res.constant)
    
    # 3. Testing whether the offset p0 is significantly different from zero.
    # If not significant, we set p_0 = 0.
    optimObjectTmp3 <- res
    optimObjectTmp3$positive.par.names <-
      setdiff(optimObjectTmp3$positive.par.names, "p_0")  
    optimObjectTmp$fixed[["p_0"]] <- 0
    res.p_0Zero.plusMinus <- getFittingResult(optimObjectTmp3)
    res.p_0Zero <- selectBest(res.p_0Zero.plusMinus)
    res <- selectSmallerModelIfDiffIsSmall(res, res.p_0Zero)
    
    intermediateResults <- list(fullModel = res.all.plusMinus,
                                TshiftFixed = res.T_shiftLB.plusMinus,
                                Constant = res.constant.plusMinus,
                                p0Zero = res.p_0Zero.plusMinus)
  } else {
    params <- setdiff(names(optimObject.orig[["lb.vec"]]), "sigma")
    statLst <- list()
    statObjLst <- list()
    for (param in params) {
      optimObjectTmp <- optimObject.orig
      optimObjectTmp$initialGuess.vec <- optimParamsFullModel
      optimObjectTmp$positive.par.names <-
        setdiff(optimObjectTmp$positive.par.names, param) 
      optimObjectTmp$fixed[[param]] <- 0
      res.param.fixed <- getFittingResult(optimObjectTmp, nInitialGuesses = 50)
      res.fixed <- selectBest(res.param.fixed)
      
      LRstat <- res.fixed$value / res$value
      difference <-  res.fixed$value - res$value
      df <- sum(is.na(res[["fixed"]])) - sum(is.na(res.fixed[["fixed"]]))
      pVal <- stats::pchisq(difference, df = df, lower.tail = FALSE)
      statLst <- append(statLst, list(list(param = param, 
                                           df = sum(is.na(res.fixed[["fixed"]])),
                                           LRstat = LRstat, 
                                           pVal = pVal)))
      statObjLst <- append(statObjLst, list(res.fixed))
    }
    names(statLst) <- names(statObjLst) <- params
    
    grDevices::pdf(file = "doseResponseRTF_parameter_waterfallPlots_forSignificanceTable.pdf", width = 12, height = 10)
    for (i in seq(length(statObjLst))){
      optimResTmpLstValuesAll <- unlist(lapply(statObjLst[[i]][["optimResults"]], function(x) unlist(x[["optimRes"]][grep("value",names(x[["optimRes"]]))])))
      print(plotWaterfallPlot(optimResTmpLstValuesAll) + ggplot2::ggtitle(names(statObjLst)[i]))
    }
    grDevices::dev.off()
    
    statLst.df <- do.call(rbind, lapply(statLst, data.frame))
    utils::write.csv(statLst.df, "doseResponseRTF_parameter_significanceTable.csv", row.names = FALSE)
  }
  
  finalModel <- res
  finalParams <- res$fitted
  
  print("The parameters of the best fit are:")
  print(paste(names(finalParams), round(finalParams, 4),
              sep = ": ", collapse = ", "))
  
  return(list(finalModel = res,
              finalParams = finalParams,
              intermediateResults = intermediateResults))
  
}
