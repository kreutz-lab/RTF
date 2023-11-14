#' Run RTF
#'
#' @description Run RTF
#' @return List of the final RTF model (finalModel), the optimized parameters
#' (finalParams), the plot of the final model (finalPlot), as well as the
#' intermediate results (intermediateResults).
#' @param data Data frame containing columns named 't' (time) and
#' 'y' (quantitative value) for modus = 'RetardedTransientDynamics' ('D') and 
#' 'ImmediateResponseFunction' ('I'), and columns 't', 'y', and 'd' (dose) for
#' modus = 'DoseDependentRetardedTransientDynamics' ('DD')
#' @param modus String indicating if modus 'RetardedTransientDynamics' ('D'), 
#' 'ImmediateResponseFunction' ('I') or
#' 'DoseDependentRetardedTransientDynamics' ('DD') should be used. 
#' If no modus is provided default setting are  
#' 'DoseDependentRetardedTransientDynamics' if column with name 'd' is present 
#' and else 'RetardedTransientDynamics'.
#' @param optimFunction String indicating the optimization function which 
#' should be used (Default: "chiSquare")
#' @param modelReduction Boolean indicating of model reduction should be performed
#' (Default: TRUE)
#' @param nInitialGuesses Integer indicating number of initial guesses (Default: 50)
#' @param control List of control arguments passed to the function stats::optim 
#' (Default: list(trace = 1, maxit = 1000, factr = 1.0e-20))
#' @export runRTF
#' @examples
#' modus <- "RetardedTransientDynamics"
#' data <- getExampleDf()
#' plotData(data)
#' res <- runRTF(data, modus = modus)

runRTF <- function(data, 
                   modus = NULL, 
                   optimFunction = "chiSquare", 
                   modelReduction = TRUE,
                   nInitialGuesses = 50,
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
  
  if (modus != "DoseDependentRetardedTransientDynamics" &
      !all(c("t", "y") %in% names(data))) {
    stop("Input data frame needs to contain the columns 't' and 'y'.")
  } else if (modus == "DoseDependentRetardedTransientDynamics" &
             !all(c("t", "y", "d") %in% names(data))) {
    stop("Input data frame needs to contain the columns 't', 'y', and 'd'.")
  }
  
  
  optimObject.orig <- initializeOptimObject(data, modus = modus, 
                                  optimFunction = optimFunction)
  res.all.plusMinus <- getFittingResult(optimObject.orig, nInitialGuesses = nInitialGuesses)
  res <- selectBest(res.all.plusMinus)
  
  optimParamsFullModel <- res[["bestOptimResult"]][["par"]]
  intermediateResults <- list()
  
  if (modelReduction) {
    if (modus != "DoseDependentRetardedTransientDynamics") {
      #  MODEL REDUCTION
      # 1. Testing whether there is time retardation,
      # i.e., if tau parameter is significantly different from the lower bound.
      #  If not significant, tau is set to the lower bound which is
      # tau = −2 by default.
      optimObjectTmp <- optimObject.orig
      # optimObjectTmp$positive.par.names <-
      #   setdiff(optimObjectTmp$positive.par.names, "tau")  # because lb.vec[["tau"]] corresponds to -2
      
      optimObjectTmp[["takeLog10"]][names(optimObjectTmp[["takeLog10"]]) == "tau"] <- FALSE
      
      optimObjectTmp$fixed[["tau"]] <- optimObject.orig$lb.vec[["tau"]]
      res.tauLB.plusMinus <- getFittingResult(optimObjectTmp, nInitialGuesses = nInitialGuesses)
      res.tauLB <- selectBest(res.tauLB.plusMinus)
      res <- selectSmallerModelIfDiffIsSmall(res, res.tauLB)
      
      # 2. Testing whether the model is in agreement with a constant.
      # If not significant, we set A = B = 0.
      optimObjectTmp2 <- res
      # optimObjectTmp2$positive.par.names <-
      #   setdiff(optimObjectTmp2$positive.par.names, c("A", "B"))  
      
      optimObjectTmp2[["takeLog10"]][names(optimObjectTmp2[["takeLog10"]]) %in% c("A", "B")] <- FALSE
      
      optimObjectTmp2$fixed[["A"]] <- optimObjectTmp2$fixed[["B"]] <- 0
      res.constant.plusMinus <- getFittingResult(optimObjectTmp2, nInitialGuesses = nInitialGuesses)
      res.constant <- selectBest(res.constant.plusMinus)
      res <- selectSmallerModelIfDiffIsSmall(res, res.constant)
      
      # 3. Testing whether the offset b is significantly different from zero.
      # If not significant, we set b = 0.
      optimObjectTmp3 <- res
      # optimObjectTmp3$positive.par.names <-
      #   setdiff(optimObjectTmp3$positive.par.names, "b")  
      
      optimObjectTmp3[["takeLog10"]][names(optimObjectTmp3[["takeLog10"]]) == "b"] <- FALSE
      optimObjectTmp3$fixed[["b"]] <- 0
      res.bZero.plusMinus <- getFittingResult(optimObjectTmp3, nInitialGuesses = nInitialGuesses)
      res.bZero <- selectBest(res.bZero.plusMinus)
      res <- selectSmallerModelIfDiffIsSmall(res, res.bZero)
      
      intermediateResults <- list(fullModel = res.all.plusMinus,
                                  tauFixed = res.tauLB.plusMinus,
                                  Constant = res.constant.plusMinus,
                                  bZero = res.bZero.plusMinus)
    } else {
      params <- setdiff(names(optimObject.orig[["lb.vec"]]), "sigma")
      statLst <- list()
      statObjLst <- list()
      for (param in params) {
        print(param)
        optimObjectTmp <- optimObject.orig
        optimObjectTmp$initialGuess.vec <- optimParamsFullModel
        
        # if parameters with reciprocal hill (equationhillEquationReciprocal) set fixed to upper bound
        # if (param %in% c("M_alpha", "M_gamma", "K_alpha", "K_gamma", "M_tau", "K_tau")) {
        if (param %in% c("M_tau", "K_tau")) {
          optimObjectTmp$fixed[[param]] <- optimObjectTmp[["ub.vec"]][names(optimObjectTmp[["ub.vec"]]) == param]
        } else {
          # optimObjectTmp$positive.par.names <-
          #   setdiff(optimObjectTmp$positive.par.names, param) 
          optimObjectTmp[["takeLog10"]][names(optimObjectTmp[["takeLog10"]]) == param] <- FALSE
          optimObjectTmp$fixed[[param]] <- 0
        }
        
        res.param.fixed <- getFittingResult(optimObjectTmp, nInitialGuesses = nInitialGuesses)
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
