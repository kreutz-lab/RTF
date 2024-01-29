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

RTF with no dose-response dependency
(Input data frame should contain the columns 't' for time and 
'y' for the quantitative value):

```
# data <- getData(file="LDH_WT.xlsx", tCol="time", 
#                 quantCols=c("Replikat1", "Replikat2", "Replikat3"))
# data <- getData()

data <- getExampleDf()
plotData(data)
res <- runRTF(data, modus = "RetardedTransientDynamics")
plotRTF(optimObject = res, fileNamePrefix = "finalModel", plotAllFits = TRUE)
```

RTF with dose-response dependency
(Input data frame should contain the columns 't' for time, 
'y' for the quantitative value, and 'd' for dose):

```
data.doseResponse <- getExampleDf(
                             modus = "DoseDependentRetardedTransientDynamics")
plotData(data.doseResponse)
res.doseResponse <- runRTF(data.doseResponse, 
                          modus = "DoseDependentRetardedTransientDynamics",
                          modelReduction = FALSE)
plotRTF(res.doseResponse, fileNamePrefix = "doseResponseFinalModel")

plotFit(par = res.doseResponse[["finalParams"]],
                 y = data.doseResponse$y, 
                 t = data.doseResponse$t, 
                 d = data.doseResponse$d, 
                 modus = 'DoseDependentRetardedTransientDynamics',
                 withData = TRUE,
                 title = " ")
```

Low-dimnensional representation of RTF parameters of 20 or more time series: 

```
data(strasenTimeSeries)
df.multipleTimeSeries <- strasenTimeSeries[, 1:20]
colNames <- colnames(df.multipleTimeSeries[2:ncol(df.multipleTimeSeries)])
metaInfo <- sub("_[^_]+$", "", colNames)
res <- getLowDimensionalRTF(df.multipleTimeSeries,
                            metaInfo = metaInfo, 
                            metaInfoName = "species",
                            fileString = "strasen_subset")

```
