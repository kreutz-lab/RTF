#' Generate plots
#'
#' @description Plot fitting results for optimObjects of modus 
#' "RetardedTransientDynamics" or "DoseDependentRetardedTransientDynamics"
#' @return ggplot object
#' @param modus "RetardedTransientDynamics" or 
#' "DoseDependentRetardedTransientDynamics"
#' @export plot
#' @examples
#' data <- getExampleDf()
#' optimObject.orig <- initializeOptimObject(
#'                 data, modus = 'RetardedTransientDynamics')
#' signum_TF <- 1
#' optimObject.orig$fixed[["signum_TF"]] <- signum_TF
#' nInitialGuesses <- 50
#' optim.res <- getMultiStartResults(
#'                 optimObject.orig, objFunct, nInitialGuesses)
#' pl <- plotMultiStartPlots(optim.res, fileNamePrefix = "ExamplePlot")

plot <- function(optimObject, modus, plotAllFits = TRUE) {
  

}
