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

### Single-dose RTF
Input data frame should contain the columns 't' for time and 
'y' for the quantitative value. 
Optionally, a column 'sigmaExp' can be provided with the standard error of 
the experimental data.

```
data.singleDose <- getSimData()
# data.singleDose <- openxlsx::read.xlsx(
#      system.file("extdata", "ExampleDataSingleDose.xlsx", package = "RTF"))

plotData(data.singleDose)
res.singleDose <- RTF(data.singleDose, modus = "singleDose")
plotRTF(optimObject = res.singleDose, 
        fileNamePrefix = "finalModel", 
        plotAllFits = TRUE)
        
# Subsequently, you can perform model reduction
res.singleDose.reduced <- modelReduction(res.singleDose$finalModel)

# A result of RTF() can be complemented with new results of RTF() on the same
# dataset
resOld.singleDose <- res.singleDose
resNew.singleDose <- RTF(data.singleDose, modus = "singleDose", 
          resOld = resOld.singleDose)
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
(Currently only possible for single-dose RTF parameters)
```
data(strasen)
df.multipleTimeSeries <- strasen[, 1:20]
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
the plotInteractive2DPlot can be used. 
By hovering over a point the corresponding time-resolved behavior is 
displayed in an additional smaller subplot.
```
data(almaden)
timeSeries <- almaden # first column needs to be "time"
colNames <- colnames(timeSeries[2:ncol(timeSeries)])
species <- sub("_[^_]+$", "", colNames)
conditionID <- gsub(".*_", "", colNames)

# # We don't run the following lines as the required object almadenModelLst is 
# # available as an example (data(almadenModelLst)).
# fileString <- "almadenExampleFile"
# params.lst <- getParamsFromMultipleTimeSeries(
#   almaden,
#   fileString = fileString,
#   saveFolderPath = tempdir(),
#   nInitialGuesses = 50
# )
# param.df <- params.lst[["param.df"]]
# RTFmodelLst <- params.lst[["RTFmodelLst"]]

data(almadenParams)
data(almadenModelLst)
param.df <- almadenParams
RTFmodelLst <- almadenModelLst

colNames <- colnames(timeSeries[2:ncol(timeSeries)])
plt <- plotInteractiveUMAP(df = timeSeries,
                           fileString = "almadenInteractiveUMAP",
                           conditions = gsub(".*_", "", colNames),
                           species = sub("_[^_]+$", "", colNames),
                           hRatio = 0.4,
                           RTFmodelLst = RTFmodelLst,
                           param.df = param.df)                             
                             
# Save to html file
htmlwidgets::saveWidget(plt, "interactiveUMAP.html")
```