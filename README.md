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

Time-dependent RTF
(Input data frame should contain the columns 't' for time and 
'y' for the quantitative value. 
Optionally, a column 'sigmaExp' can be provided with the standard error of 
the experimental data.):

```
data <- getSimData()
# data <- openxlsx::read.xlsx(
#      system.file("extdata", "ExampleDataTimeDependent.xlsx", package = "RTF"))

plotData(data)
res <- RTF(data, modus = "timeDependent")
plotRTF(optimObject = res, fileNamePrefix = "finalModel", plotAllFits = TRUE)

# A result of RTF() can be complemented with new results of RTF() on the same
# dataset
resOld <- res
res <- RTF(data, modus = "timeDependent", resOld = resOld)

# Subsequently, you can perform model reduction
res.reduced <- modelReduction(res$finalModel)
```

Dose-dependent RTF
(Input data frame should contain the columns 't' for time, 
'y' for the quantitative value, and 'd' for dose. 
Optionally, a column 'sigmaExp' can be provided with the standard error of 
the experimental data.):

```
data.doseResponse <- getSimData(modus = "doseDependent")
# data.doseResponse <- openxlsx::read.xlsx(
#      system.file("extdata", "ExampleDataDoseDependent.xlsx", package = "RTF"))

plotData(data.doseResponse)
res.doseResponse <- RTF(data.doseResponse, modus = "doseDependent")
plotRTF(res.doseResponse, fileNamePrefix = "doseResponseFinalModel")

plotFit(par = res.doseResponse[["finalParams"]],
        y = data.doseResponse$y, 
        t = data.doseResponse$t, 
        d = data.doseResponse$d, 
        modus = 'doseDependent',
        withData = TRUE,
        title = " ")
                 
res.doseResponse.reduced <- modelReduction(res.doseResponse$finalModel)
```

Low-dimensional representation of RTF parameters of 20 or more time series: 

```
data(strasenTimeSeries)
df.multipleTimeSeries <- strasenTimeSeries[, 1:20]
colNames <- colnames(df.multipleTimeSeries[2:ncol(df.multipleTimeSeries)])
metaInfo <- sub("_[^_]+$", "", colNames)
res <- lowDimensionalRTF(df.multipleTimeSeries,
                         metaInfo = metaInfo, 
                         metaInfoName = "species",
                         fileString = "strasen_subset")

```
