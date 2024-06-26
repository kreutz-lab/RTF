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

### Low-dimensional representation of multiple (at least 20) fitted RTFs
(Currently only possible for single-dose RTF parameters)
```
data(strasen)
df.multipleTimeCourses <- strasen[, 1:20]
colNames <- colnames(df.multipleTimeCourses[2:ncol(df.multipleTimeCourses)])
metaInfo <- sub("_[^_]+$", "", colNames)
res <- lowDimensionalRTF(df.multipleTimeCourses,
                         metaInfo = metaInfo, 
                         metaInfoName = "Species",
                         fileString = "strasen_subset")
                         
# Save plots
ggplot2::ggsave(filename = "test.pdf", plot = res[["plots"]],
                width = 10, height = 30)

```

To generates an interactive UMAP plot based on the RTF parameters for 
multiple time courses plotInteractiveUMAP() can be used, where each point 
corresponds to a single time course. 
By hovering over a point the corresponding time-resolved behavior is displayed 
in an additional smaller subplot.
```
data(almaden)
timeCourses <- almaden # first column needs to be "time"
colNames <- colnames(timeCourses[2:ncol(timeCourses)])
species <- sub("_[^_]+$", "", colNames)
conditionID <- gsub(".*_", "", colNames)

# # We don't run the following lines as the required object almadenModelLst is 
# # available as an example (data(almadenModelLst)).
# fileString <- "almadenExampleFile"
# params.lst <- getParamsFromMultipleTimeCourses(
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

colNames <- colnames(timeCourses[2:ncol(timeCourses)])
plt <- plotInteractiveUMAP(df = timeCourses,
                           fileString = "almadenInteractiveUMAP",
                           conditions = gsub(".*_", "", colNames),
                           species = sub("_[^_]+$", "", colNames),
                           takeRank = TRUE,
                           seed = 222,
                           vRatio = 0.05,
                           hRatio = 0.35,
                           RTFmodelLst = RTFmodelLst,
                           param.df = param.df)                     
                             
# Save to html file
htmlwidgets::saveWidget(plt, "interactiveUMAP.html")
```