# RTF

library(RTF)

modus <- "RetardedTransientDynamics"

plot <- TRUE

data <- getExampleDf()

plot(data)

res <- runRTF(data, modus = modus, plot = plot)
