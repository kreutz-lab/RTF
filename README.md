# Retarded Transient Function (RTF)

This package is based on the Retarded Transient Function (RTF) introduced in the publication 

[Kreutz C (2020) A New Approximation Approach for Transient Differential Equation Models. Front. Phys. 8:70.](https://doi.org/10.3389/fphy.2020.00070)

## Installation
```
install.packages("devtools")
devtools::install_github("kreutz-lab/RTF")
```

## Examples
```
library(RTF)
```

### Time-dependent RTF
Input data frame should contain the columns 't' for time and 
'y' for the quantitative value. 
Optionally, a column 'sigmaExp' can be provided with the standard error of 
the experimental data.

```
data.timeDependent <- getSimData()
# data.timeDependent <- openxlsx::read.xlsx(
#      system.file("extdata", "ExampleDataTimeDependent.xlsx", package = "RTF"))

plotData(data.timeDependent)
res.timeDependent <- RTF(data, modus = "timeDependent")
plotRTF(optimObject = res.timeDependent, 
        fileNamePrefix = "finalModel", 
        plotAllFits = TRUE)

# A result of RTF() can be complemented with new results of RTF() on the same
# dataset
resOld.timeDependent <- res.timeDependent
res <- RTF(data.timeDependent, modus = "timeDependent", 
          resOld = resOld.timeDependent)

# Subsequently, you can perform model reduction
res.timeDependent.reduced <- modelReduction(res.timeDependent$finalModel)
```

### Dose-dependent RTF
Input data frame should contain the columns 't' for time, 
'y' for the quantitative value, and 'd' for dose. 
Optionally, a column 'sigmaExp' can be provided with the standard error of 
the experimental data.

```
data.doseDependent <- getSimData(modus = "doseDependent")
# data.doseDependent <- openxlsx::read.xlsx(
#      system.file("extdata", "ExampleDataDoseDependent.xlsx", package = "RTF"))

plotData(data.doseDependent)
res.doseDependent <- RTF(data.doseDependent, modus = "doseDependent")
plotRTF(res.doseDependent, fileNamePrefix = "doseDependentFinalModel")

plotFit(par = res.doseDependent[["finalParams"]],
        y = data.doseDependent$y, 
        t = data.doseDependent$t, 
        d = data.doseDependent$d, 
        modus = 'doseDependent',
        withData = TRUE,
        title = " ")
                 
res.doseDependent.reduced <- modelReduction(res.doseDependent$finalModel)
```

### Low-dimensional representation of RTF parameters of 20 or more time series 
(Currently only possible for time-dependent RTF parameters)
```
data(strasenTimeSeries)
df.multipleTimeSeries <- strasenTimeSeries[, 1:20]
colNames <- colnames(df.multipleTimeSeries[2:ncol(df.multipleTimeSeries)])
metaInfo <- sub("_[^_]+$", "", colNames)
res <- lowDimensionalRTF(df.multipleTimeSeries,
                         metaInfo = metaInfo, 
                         metaInfoName = "Species",
                         fileString = "strasen_subset")
                         
# Save plots
ggplot2::ggsave(filename = "test.pdf", plot = res[["plots"]],
                width = 10, height = 30)

```

To generates a dynamic 2D plot based on the RTF parameters for multiple time 
series, where each point corresponds to a single time series, 
the plotDynamic2DPlot can be used. 
By hovering over a point the corresponding time-resolved behavior is 
displayed in an additional smaller subplot.
```
data(almadenTimeSeries)
data(almadenParams)
data(almadenResLst)
timeSeries <- almadenTimeSeries # first column needs to be "time"
colNames <- colnames(timeSeries[2:ncol(timeSeries)])
species <- sub("_[^_]+$", "", colNames)
conditionID <- gsub(".*_", "", colNames)

# # Besides returning the fitted parameters, this function also generates 
# # a RDS with the final models, which is needed later on
# fileString <- "testFile"
# param.df <- getParamsFromMultipleTimeSeries(
#   almadenTimeSeries,
#   fileString = fileString,
#   saveFolderPath = tempdir(),
#   nInitialGuesses = 50
# )
# res.lst <- readRDS(file = paste0(tempdir(), "/", fileString, ".RDS"))

plots <- plotLowDimensionalRTF(
  df = almadenParams,
  metaInfo = species, 
  metaInfoName = "Species",
  maxTime = max(timeSeries$time),
  numClust = NULL
)
    
df.umapData <- plots[["umap.data"]]
clustID <- df.umapData$clustID
ID <- row.names(df.umapData)
plt <- plotDynamic2DPlot(dim1Vec = df.umapData$UMAP1,
                         dim2Vec = df.umapData$UMAP2,
                         ID = ID,
                         species = species,
                         res.lst = almadenResLst,
                         metaInfo = conditionID, # alternatively: clustID
                         hRatio = 0.4, 
                         vRatio = 0
)
```