source("StatisticaHypothesisTesting/Common.R", encoding = "UTF-8")
source("StatisticaHypothesisTesting/OneSampleTests.R", encoding = "UTF-8")
source("StatisticaHypothesisTesting/TwoSamplesTests.R", encoding = "UTF-8")
source("Regression/Regression.R", encoding = "UTF-8")

MyData <- read.csv2(file = "data.csv", header = FALSE, sep = ";")

KolmogorovSmirnovTest(MyData$V1, 61, 9, 0.05)
varTestPValue = TwoSampleFTest(MyData$V1, MyData$V2, "two.sided", 0.05)
equalVariances = FALSE
if (varTestPValue > 0.05) {
  equalVariances = TRUE
}
TwoSamleTTest(MyData$V1, MyData$V2, "two.sided", 0.05, FALSE, equalVariances)
x0 = readline(prompt="Enter x0: ")
Regression(MyData$V3, MyData$V4, 0.9, 0.9, as.integer(x0))

