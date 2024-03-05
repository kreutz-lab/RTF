#' Generates dynamic UMAP plot based on the RTF parameters for multiple time 
#' series
#'
#' @description Generates dynamic UMAP plot based on the RTF parameters for 
#' multiple time series, where each point corresponds to a single time series. 
#' By hovering over a point the corresponding time-resolved behavior is 
#' displayed in an additional smaller subplot.
#' @return Plotly object of the dynamic UMAP plot.
#' @param timeSeries Data frame with the first column named 'time' defining the
#' different time points and all the following columns corresponding to the
#' different time series.
#' @param param.df Data frame with the RTF parameter values. 
#' Its columns 'alpha', 'gamma', 'A', 'B', 'b', 'tau', and 'signum_TF'
#' represent the different RTF parameters. The rows correspond to the different
#' time series. 
#' @param res.lst List with the RTF result for each time series
#' @param species Vector of the names of molecular species, which needs to have 
#' the same length as the number of rows of param.df.
#' @param metaInfo (Optional) Vector specifying additional meta information for
#' each time series. Needs to have the same length as the number of rows of 
#' param.df.
#' @param positionLeft Integer specifying the horizontal position of the subplot 
#' (Default: 0)
#' @param positionTop Integer specifying the vertical position of the subplot 
#' (Default: 0)
#' @param numClust (Optional) Integer indicating the number of clusters used 
#' for k-means clustering. If not specified, number of clusters will be 
#' determined automatically using the function NbClust::NbClust().
#' @export getDynamicUMAP
#' @examples
#' \dontrun{
#' data(almadenTimeSeries)
#' data(almadenParams)
#' data(almadenResLst)
#' timeSeries <- almadenTimeSeries # first column needs to be "time"
#' colNames <- colnames(timeSeries[2:ncol(timeSeries)])
#' species <- sub("_[^_]+$", "", colNames)
#' metaInfo <- gsub(".*_", "", colNames)
#' 
#' # # Besides returning the fitted parameters, this function also generates 
#' # # a RDS with the final models, which is needed later on
#' # fileString <- "testFile"
#' # param.df <- getParamsFromMultipleTimeSeries(
#' #   almadenTimeSeries,
#' #   fileString = fileString,
#' #   saveFolderPath = tempdir(),
#' #   nInitialGuesses = 100
#' # )
#' # res.lst <- readRDS(file = paste0(tempdir(), "/", fileString, ".RDS"))
#' 
#' plt <- getDynamicUMAP(timeSeries = timeSeries, param.df = almadenParams, 
#'                       res.lst = almadenResLst, 
#'                       species = species, metaInfo = metaInfo)
#' }

getDynamicUMAP <- function(timeSeries, param.df, res.lst, 
                           species, metaInfo = c(), 
                           positionLeft = 0, positionTop = 0, 
                           numClust = NULL) {
  
  plots <- getLowDimensionalRTFPlots(
    df = param.df,
    metaInfo = "", 
    metaInfoName = "species",
    maxTime = max(timeSeries$time),
    numClust = numClust)
  
  umapData <- plots[["umap.data"]]
  umapData$speciesID <- species
  
  plotRTFForAllTimeSeries(
    res.lst,
    fileString = "",
    saveFolderPath = tempdir(),
    plotFitsToSingleFile = FALSE,
    plotFitsToSingleFileExtension = "jpeg",
    plotFitOnly = TRUE,
    height = 7,
    width = 7
  )
  
  umapData$speciesID_underscore <- gsub("/", "_", umapData$speciesID)
  umapData$IMG_PATH <- 
    paste0(tempdir(), "/modelPlot_", umapData$speciesID_underscore, ".jpeg")
  umapData$IMG_PATH2 <- 
    sapply(umapData$IMG_PATH, function(x) {base64enc::dataURI(file = x )})
  
  ############################

  d3 <- htmltools::htmlDependency(
    "d3", "7.8.5",
    src = normalizePath("../js"),
    script = "d3.v7.min.js"
  )
  
  js <- '
  function(el, x, data) {
    var tooltip = d3.select("#" + el.id + " .svg-container")
      .append("div")
      .attr("class", "my-custom-tooltip");
  
    el.on("plotly_hover", function(d) {
      var pt = d.points[0];
      var img = "<img src=\\\"" +  pt.customdata + "\\\" width=150>";
      tooltip.html(img)
        .style("position", "absolute")
        .style("left", data.left + "px")
        .style("top", data.top + "px");
      tooltip.transition()
        .duration(300)
        .style("opacity", 1);
    });
  
    el.on("plotly_unhover", function(d) {
      tooltip.transition()
        .duration(500)
        .style("opacity", 0);
    });
  }
  '
  ##########
  colorID <- "clustID" 
  
  if (length(metaInfo) > 0) {
    umapData <- data.frame(umapData, metaInfo = metaInfo)
  }

  plt2 <- plotly::plot_ly(data = umapData) %>%
    # Add line connecting two points of same species
    plotly::add_trace(
      x = ~UMAP1, y = ~UMAP2, split = ~species, opacity = 0.2,
      line = list(color = "black"),
      type = "scatter", mode = "lines", showlegend = FALSE, inherit = FALSE)
  
  if (length(metaInfo) > 0) {
    colorID <- "metaInfo"
    hasOnlyTwoValues <- length(unique(umapData$metaInfo)) == 2
  } else {
    colorID <- "clustID"
    hasOnlyTwoValues <- length(unique(umapData$colorID)) == 2
  }
  
  if (hasOnlyTwoValues) {
    plt2 <- plt2 %>%  plotly::add_trace(
      x = ~UMAP1, y = ~UMAP2, color =  ~get(colorID), 
      type = "scatter", mode = "markers",
      hoverinfo = ~speciesID, customdata = ~IMG_PATH2,
      text = ~speciesID,
      marker = list(color = ifelse(
        umapData[, colorID] ==  unique(umapData[, colorID])[1], "red", "blue"))) 
  } else {
    plt2 <- plt2 %>%  plotly::add_trace(
      x = ~UMAP1, y = ~UMAP2, color =  ~get(colorID), 
      type = "scatter", mode = "markers",
      hoverinfo = ~speciesID, customdata = ~IMG_PATH2,
      text = ~speciesID) 
  }
  
  # modify 'left' and 'top' to adjust position of image
  plt2 <- plt2 %>% htmlwidgets::onRender(
    js, data = list(left = positionLeft, top = positionTop))
  
  plt2$dependencies <- c(plt2$dependencies, list(d3))
  
  plt2
}