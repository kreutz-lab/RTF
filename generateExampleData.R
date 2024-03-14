library(openxlsx)
library(RTF)

SE <- function(x) sd(x)/3

################################################################################
# Example dataset for time-dependent RTF
replicate1 <- getSimData(modus = "timeDependent", noise = 0.8)
replicate2 <- getSimData(modus = "timeDependent", noise = 0.8)
replicate3 <- getSimData(modus = "timeDependent", noise = 0.8)

exampleDataTimeDependent <- data.frame(
  t = replicate1$t, 
  y = apply(data.frame(replicate1$y, replicate2$y, replicate3$y), 1, mean),
  sigmaExp = apply(data.frame(replicate1$y, replicate2$y, replicate3$y), 1, SE)
  )

openxlsx::write.xlsx(
  x = exampleDataTimeDependent, file = "ExampleDataTimeDependent.xlsx")

################################################################################
# Example dataset for dose-dependent RTF
doseReplicate1 <- getSimData(modus = "doseDependent", noise = 0.06)
doseReplicate2 <- getSimData(modus = "doseDependent", noise = 0.06)
doseReplicate3 <- getSimData(modus = "doseDependent", noise = 0.06)

exampleDataDoseDependent <- data.frame(
  t = doseReplicate1$t, 
  d = doseReplicate1$d,
  y = apply(
    data.frame(doseReplicate1$y, doseReplicate2$y, doseReplicate3$y), 1, mean),
  sigmaExp = apply(
    data.frame(doseReplicate1$y, doseReplicate2$y, doseReplicate3$y), 1, SE)
  )

openxlsx::write.xlsx(
  x = exampleDataDoseDependent, file = "ExampleDataDoseDependent.xlsx")

session <- sessionInfo()
sink(paste0("generateExampleData_sessionInfo.txt"))
print(session)
sink()
