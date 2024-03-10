#' Run model reduction on full RTF model
#'
#' @description Run model reduction on full RTF model
#' @return RTF model after model reduction
#' @param res Full RTF model, i.e., generated without fixed parameters
#' @param nInitialGuesses Integer indicating number of initial guesses
#' (Default: 100)
#' @param plotAllPointsWaterfall Boolean indicating if all points should be 
#' plotted in waterfall plot (Default: FALSE). 
#' If FALSE, all values up to the median of those values are plotted.
#' @export modelReduction
#' @examples
#' modus <- "timeDependent"
#' data <- getExampleDf()
#' plotData(data)
#' res <- runRTF(data, modus = modus)
#' res.reduced <- modelReduction(res$finalModel)

modelReduction <- function(res, 
                           nInitialGuesses = 100, 
                           plotAllPointsWaterfall = FALSE) {
  modus <- res[["modus"]]
  res.orig <- res
  
  optimParamsFullModel <- res[["bestOptimResult"]][["par"]]
  optimParamsFullModel <- optimParamsFullModel[
    names(optimParamsFullModel) != "signum_TF"]
  
  res.orig$initialGuess.vec <- optimParamsFullModel
  
  if (modus != "doseDependent") {
    #  MODEL REDUCTION
    # 1. Testing whether there is time retardation,
    # i.e., if tau parameter is significantly different from the lower bound.
    #  If not significant, tau is set to the lower bound which is
    # tau = −2 by default.
    optimObjectTmp <- res.orig
    optimObjectTmp[["takeLog10"]][
      names(optimObjectTmp[["takeLog10"]]) == "tau"] <- FALSE
    optimObjectTmp$fixed[["tau"]] <- res.orig$lb.vec[["tau"]]
    res.tauLB <- getInitialGuessResults(
      optimObjectTmp, nInitialGuesses = nInitialGuesses)
    res <- selectSmallerModelIfDiffIsSmall(res, res.tauLB)
    
    # 2. Testing whether the model is in agreement with a constant.
    # If not significant, we set A = B = 0.
    optimObjectTmp2 <- res
    optimObjectTmp2[["takeLog10"]][names(optimObjectTmp2[["takeLog10"]]) %in%
                                     c("A", "B")] <- FALSE
    optimObjectTmp2$fixed[["A"]] <- optimObjectTmp2$fixed[["B"]] <- 0
    res.constant <- getInitialGuessResults(
      optimObjectTmp2, nInitialGuesses = nInitialGuesses)
    res <- selectSmallerModelIfDiffIsSmall(res, res.constant)
    
    # 3. Testing whether the offset b is significantly different from zero.
    # If not significant, we set b = 0.
    optimObjectTmp3 <- res
    optimObjectTmp3[["takeLog10"]][
      names(optimObjectTmp3[["takeLog10"]]) == "b"] <- FALSE
    optimObjectTmp3$fixed[["b"]] <- 0
    res.bZero <- getInitialGuessResults(
      optimObjectTmp3,nInitialGuesses = nInitialGuesses)
    res <- selectSmallerModelIfDiffIsSmall(res, res.bZero)
    
    finalParams <- res$fitted

    print("The parameters of the best fit after model reduction are:")
    print(paste(
      names(finalParams),
      signif(finalParams, 4),
      sep = ": ",
      collapse = ", "
    ))
    print("OptimValue:")
    print(res[["bestOptimResult"]][["value"]])
    
    reductionResults <- list(finalModel = res, finalParams = finalParams)
    
  } else {
    RTFparams <- c("alpha", "gamma", "A", "B", "tau")
    statLst <- list()
    statObjLst <- list()
    
    for (RTFparam in RTFparams) {
      print(RTFparam)
      optimObjectTmp <- res.orig
      
      optimObjectTmp$fixed[[paste0("h_", RTFparam)]] <- 1
      
      optimObjectTmp[["takeLog10"]][
        names(optimObjectTmp[["takeLog10"]]) == paste0("K_", RTFparam)] <- FALSE
      optimObjectTmp$fixed[[paste0("K_", RTFparam)]] <- 0
      
      res.fixed <- getInitialGuessResults(optimObjectTmp,
                                          nInitialGuesses = nInitialGuesses)
      
      difference <-  res.fixed$value - res$value
      df <- sum(is.na(res[["fixed"]])) - sum(is.na(res.fixed[["fixed"]]))
      pVal <- stats::pchisq(difference, df = df, lower.tail = FALSE)
      statLst <- append(statLst,
                        list(list(
                          RTFparam = RTFparam,
                          df = df,
                          LRTstat = difference,
                          pVal = pVal
                        )))
      statObjLst <- append(statObjLst, list(res.fixed))
    }
    names(statLst) <- names(statObjLst) <- RTFparams
    
    reductionResults <- do.call(rbind, lapply(statLst, data.frame))
  }
  
  reductionResults
}