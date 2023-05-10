setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("transientFunction_library.R")

modus <- "RetardedTransientDynamics"
plot <- TRUE

# data <- getData(file="LDH_WT.xlsx", tCol="time", quantCols=c("Replikat1", "Replikat2", "Replikat3")) # "LDH_WT.xlsx" , "ExampleData.xls"
# data <- getData()

data <- getExampleDf()
plot(data)
res <- runRTF(data, modus = modus, plot = plot)

