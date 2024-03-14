#' Generates dynamic 2D plot based on the RTF parameters for multiple time 
#' series
#'
#' @description Generates dynamic 2D plot based on the RTF parameters for 
#' multiple time series, where each point corresponds to a single time series. 
#' By hovering over a point the corresponding time-resolved behavior is 
#' displayed in an additional smaller subplot.
#' @return Plotly object of the dynamic 2D plot.
#' @param dim1Vec Vector with values of first dimension.
#' @param dim2Vec Vector with values of second dimension. Has same length of 
#' dim1Vec
#' @param ID Vector with unique identifiers with same length as dim1Vec and 
#' dim2Vec.
#' @param species Vector with names of molecular species with same length as 
#' dim1Vec and dim2Vec.
#' @param res.lst List with the RTF result for each time series
#' @param metaInfo (Optional) Vector specifying  meta information for
#' each time series. Needs to have the same length as the number of rows of 
#' param.df.
#' @param plotLines Boolean indicating if lines should be plotted between 
#' species of the same name (Default: TRUE)
#' @param hRatio Float between 0 and 1 indicating where subplot should be placed
#' horizontally in relation to plot width (Default: 0).
#' @param vRatio Float between 0 and 1 indicating where subplot should be placed
#' vertically in relation to plot height (Default: 0).
#' @export plotDynamic2DPlot
#' @examples
#' \dontrun{
#' data(almadenTimeSeries)
#' data(almadenParams)
#' data(almadenResLst)
#' timeSeries <- almadenTimeSeries # first column needs to be "time"
#' colNames <- colnames(timeSeries[2:ncol(timeSeries)])
#' species <- sub("_[^_]+$", "", colNames)
#' conditionID <- gsub(".*_", "", colNames)
#' 
#' # # Besides returning the fitted parameters, this function also generates 
#' # # a RDS with the final models, which is needed later on
#' # fileString <- "testFile"
#' # param.df <- getParamsFromMultipleTimeSeries(
#' #   almadenTimeSeries,
#' #   fileString = fileString,
#' #   saveFolderPath = tempdir(),
#' #   nInitialGuesses = 50
#' # )
#' # res.lst <- readRDS(file = paste0(tempdir(), "/", fileString, ".RDS"))
#' 
#' numClust <- NULL
#' plots <- plotLowDimensionalRTF(
#'   df = almadenParams,
#'   metaInfo = species, 
#'   metaInfoName = "species",
#'   maxTime = max(timeSeries$time),
#'   numClust = numClust
#' )
#'     
#' df.umapData <- plots[["umap.data"]]
#' clustID <- df.umapData$clustID
#' ID <- row.names(df.umapData)
#' plt <- plotDynamic2DPlot(dim1Vec = df.umapData$UMAP1,
#'                         dim2Vec = df.umapData$UMAP2,
#'                         ID = ID,
#'                         species = species,
#'                         res.lst = almadenResLst,
#'                         metaInfo = conditionID, # alternatively: clustID
#'                         hRatio = 0.4, 
#'                         vRatio = 0
#' )
#' }

plotDynamic2DPlot <- function(dim1Vec,
                             dim2Vec,
                             ID,
                             species,
                             res.lst, 
                             metaInfo = c(), 
                             plotLines = TRUE,
                             hRatio = 0, 
                             vRatio = 0) {
  
  df <- data.frame(Dim1 = dim1Vec, Dim2 = dim2Vec, ID = ID, species = species)
  tmpDir <- tempdir()
  
  plotRTFOnMultipleTimeSeries(
    res.lst,
    fileString = "",
    saveFolderPath = tmpDir,
    plotFitsToSingleFile = FALSE,
    plotFitsToSingleFileExtension = "jpeg",
    plotFitOnly = TRUE,
    height = 7,
    width = 7
  )
  
  df$ID_underscore <- gsub("/", "_", df$ID)
  df$IMG_PATH <- 
    paste0(tmpDir, "/modelPlot_", df$ID_underscore, ".jpeg")
  df$IMG_PATH2 <- 
    sapply(df$IMG_PATH, function(x) {base64enc::dataURI(file = x )})
  
  ############################

  d3 <- htmltools::htmlDependency(
    "d3", "7.8.5",
    src = dirname(system.file("extdata", "d3.v7.min.js", package = "RTF")),
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
      var h = window.innerHeight;
      var w = window.innerWidth;
      tooltip.html(img)
        .style("position", "absolute")
        .style("left", (data.hRatio * w) + "px")
        .style("top", (data.vRatio * h) + "px");
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

  if (length(metaInfo) > 0) {
    df <- data.frame(df, metaInfo = metaInfo)
  } 
  
  plt <- plotly::plot_ly(data = df) 
  
  if (plotLines) {
    plt <- plt %>%
      # Add line connecting two points of same species
      plotly::add_trace(
        x = ~Dim1, y = ~Dim2, split = ~species, opacity = 0.2,
        line = list(color = "black"),
        type = "scatter", mode = "lines", showlegend = FALSE, inherit = FALSE)
  }
  
  if (length(metaInfo) == 0) {
    plt <- plt %>%  plotly::add_trace(
      x = ~Dim1, y = ~Dim2, color = I("black"), 
      type = "scatter", mode = "markers",
      hoverinfo = ~ID, customdata = ~IMG_PATH2,
      text = ~ID) 
  } else if (length(unique(df$metaInfo)) == 2) {
    plt <- plt %>%  plotly::add_trace(
      x = ~Dim1, y = ~Dim2, color = ~metaInfo, 
      type = "scatter", mode = "markers",
      hoverinfo = ~ID, customdata = ~IMG_PATH2,
      text = ~ID,
      marker = list(color = ifelse(
        df$metaInfo == unique(df$metaInfo)[1], "red", "blue"))) 
  } else {
    plt <- plt %>%  plotly::add_trace(
      x = ~Dim1, y = ~Dim2, color = ~metaInfo, 
      type = "scatter", mode = "markers",
      hoverinfo = ~ID, customdata = ~IMG_PATH2,
      text = ~ID) 
  }
  
  # modify 'left' and 'top' to adjust position of image
  plt <- plt %>% htmlwidgets::onRender(
    js, data = list(hRatio = hRatio, vRatio = vRatio))
  
  plt$dependencies <- c(plt$dependencies, list(d3))
  
  # Delete temporary directory content
  unlink(dir(path = tmpDir, full.names = TRUE), recursive = TRUE)
  
  plt
}