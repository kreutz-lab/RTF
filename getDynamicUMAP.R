
# library(plotly)
# library(RTF)

# get molecular data
load("almadenParams.rda")
head(almadenParams)
saveRDS(almadenParams, "almadenParams.RDS")

data(almadenTimeSeries)
timeSeries <- almadenTimeSeries # first column needs to be "time"
param.df <- almadenParams
colNames <- colnames(timeSeries[2:ncol(timeSeries)])
metaInfo <- sub("_[^_]+$", "", colNames)

almadenParams <-
  RTF::getParamsFromMultipleTimeSeries(
    almadenTimeSeries,
    fileString = fileString,
    nInitialGuesses = 100
  )


plots <- getLowDimensionalRTFPlots(
  df = param.df,
  metaInfo = metaInfo, 
  metaInfoName = "species",
  maxTime = max(timeSeries$time),
  numClust = 11)

umapData <- plots[["umap.data"]]
umapData$speciesID <- row.names(umapData)
umapData$conditionID <- gsub(".*_", "", umapData$speciesID)

res.lst <- readRDS(file = paste0(fileString, ".RDS"))

plotRTFForAllTimeSeries(
  res.lst,
  fileString = fileString,
  plotFitsToSingleFile = FALSE,
  plotFitsToSingleFileExtension = "jpeg",
  plotFitOnly = TRUE,
  height = 7,
  width = 7
)

umapData$speciesID_underscore <- gsub("/", "_", umapData$speciesID)
umapData$IMG_PATH <- paste0(getwd(), "/modelPlot_", umapData$speciesID_underscore, ".jpg")
umapData$IMG_PATH2 <- sapply(umapData$IMG_PATH, function(x) {base64enc::dataURI(file = x )})


############################
# https://stackoverflow.com/a/77043374

# library(plotly)

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
    // var x = pt.xaxis.range[0];
    // var y = pt.yaxis.range[1];
    // var xPixel = pt.xaxis.l2p(x) + pt.xaxis._offset;
    // var yPixel = pt.yaxis.l2p(y) + pt.yaxis._offset;
    var img = "<img src=\\\"" +  pt.customdata + "\\\" width=150>";
    tooltip.html(img)
      .style("position", "absolute")
      // .style("left", xPixel + "px")
      // .style("top", yPixel + "px");
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

# fig <- plotly::plot_ly(umapData, x = ~UMAP1, y = ~UMAP2, color = ~clustID, 
#                mode = "markers", type = "scatter",
#                hoverinfo = ~speciesID, customdata = ~IMG_PATH2,
#                text = ~speciesID) %>%
#   htmlwidgets::onRender(js)
# 
# fig$dependencies <- c(fig$dependencies, list(d3))
# 
# fig


##########
colorID <- "clustID" # "clustID", "conditionID"
plt2 <- plotly::plot_ly(data = umapData) %>%
  # Add line connecting two poinrs of same species
  plotly::add_trace(x = ~UMAP1, y = ~UMAP2, split = ~species, opacity = 0.2,
                    line = list(color = "black"),
                    type = "scatter", mode = "lines", showlegend = FALSE, inherit = FALSE)

if (colorID == "conditionID") {
  plt2 <- plt2 %>%  plotly::add_trace(x = ~UMAP1, y = ~UMAP2, color = ~get(colorID), 
                                      type = "scatter", mode = "markers",
                                      hoverinfo = ~speciesID, customdata = ~IMG_PATH2,
                                      text = ~speciesID,
                                      marker = list(color = ifelse(umapData$conditionID == "1", "red", "blue"))) 
} else {
  plt2 <- plt2 %>%  plotly::add_trace(x = ~UMAP1, y = ~UMAP2, color = ~get(colorID), 
                                      type = "scatter", mode = "markers",
                                      hoverinfo = ~speciesID, customdata = ~IMG_PATH2,
                                      text = ~speciesID) 
}

# modify 'left' and 'top' to adjust position of image
left <- 0
top <- 0
plt2 <- plt2 %>% htmlwidgets::onRender(js, data = list(left = left, top = top))

plt2$dependencies <- c(plt2$dependencies, list(d3))

plt2