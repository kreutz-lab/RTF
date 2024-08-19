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
The input data frame for the single-dose RTF should contain the columns 
't' for time and 'y' for the quantitative value. 
An example data frame can be generated via getSimData(modus = "singleDose").
Optionally, a column 'sigmaExp' can be provided with the standard error of 
the experimental data.

```
data.singleDose <- getSimData(modus = "singleDose")
# data.singleDose <- openxlsx::read.xlsx(
#      system.file("extdata", "ExampleDataSingleDose.xlsx", package = "RTF"))

# Plot input data
plotData(data.singleDose)

# Run RTF
res.singleDose <- RTF(data.singleDose, modus = "singleDose")

# Plot fitted RTF
plotRTF(optimObject = res.singleDose, 
        fileNamePrefix = "finalModel", 
        plotAllFits = TRUE)

# Parameters can also become modified manually and the resulting fit can be 
# assessed in relation to the input data points using the function plotFit().
modifiedParams <- res.singleDose[["finalParams"]]
modifiedParams["A"] <- 2
plotFit(par = modifiedParams,
        y = data.singleDose$y, 
        t = data.singleDose$t, 
        modus = 'singleDose',
        withData = TRUE,
        title = " ")
        
# Subsequently, a model reduction can be performed using the function 
# modelReduction()
res.singleDose.reduced <- modelReduction(res.singleDose$finalModel)

# If the RTF() function is applied for a second time on the same input, e.g.,
# to improve the fit, the result of the first RTF() run can be complemented 
# with the new results of the second RTF() run by means of the function argument 
# 'resOld'. 
resOld.singleDose <- res.singleDose
resNew.singleDose <- RTF(data.singleDose, modus = "singleDose", 
          resOld = resOld.singleDose)
```

### Dose-dependent RTF
The input data frame for the dose-dependent RTF should contain the columns 
't' for time, 'y' for the quantitative value, and 'd' for dose. 
An example data frame can be generated via getSimData(modus = "doseDependent").
Optionally, a column 'sigmaExp' can be provided with the standard error of 
the experimental data.

```
data.doseDependent <- getSimData(modus = "doseDependent")
# data.doseDependent <- openxlsx::read.xlsx(
#      system.file("extdata", "ExampleDataDoseDependent.xlsx", package = "RTF"))

plotData(data.doseDependent)
res.doseDependent <- RTF(data.doseDependent, modus = "doseDependent")
plotRTF(res.doseDependent, fileNamePrefix = "doseDependentFinalModel")
```

The functions plotFit() and modelReduction() can be applied for the 
dose-dependent RTF results analogous to the single-dose RTF. However, 
for applying plotFit() to the the dose-dependent RTF results, the dose vector 
also has to be provided via d = data.doseDependent$d.


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