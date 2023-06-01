# Retarded Transient Function (RTF)

This package is based on the Retarded Transient Function (RTF) introduced in the publication 

Kreutz C (2020) A New Approximation Approach for Transient Differential Equation Models. Front. Phys. 8:70. doi: 10.3389/fphy.2020.00070

## Installation
```
install.packages("devtools")
devtools::install_github("kreutz-lab/RTF")
```

## Examples
```
library(RTF)

modus <- "RetardedTransientDynamics"
plot <- TRUE

# data <- getData(file="LDH_WT.xlsx", tCol="time", quantCols=c("Replikat1", "Replikat2", "Replikat3")) # "LDH_WT.xlsx" , "ExampleData.xls"
# data <- getData()

data <- getExampleDf()
plot(data)
res <- runRTF(data, modus = modus, plot = plot)
```
