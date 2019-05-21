source("StatisticaHypothesisTesting/Common.R", encoding = "UTF-8")
source("StatisticaHypothesisTesting/OneSampleTests.R", encoding = "UTF-8")
source("StatisticaHypothesisTesting/TwoSamplesTests.R", encoding = "UTF-8")
source("Regression/Regression.R", encoding = "UTF-8")

MyData <- read.csv(file = "data.csv", header = FALSE, sep = ",")

#TwoSamleTTest(MyData$V1, MyData$V2, "two.sided", 0.05, pairedSamples = TRUE, equalVariance = TRUE)
#KolmogorovSmirnovTest(MyData$V1, 61, 9, 0.05)
#TwoSampleFTest(MyData$V1, MyData$V2, "two.sided", 0.05)

Regression(MyData$V1, MyData$V2, alpha1 = 0.9, alpha2 = 0.95, xest = 30.0)

#IndependenceTest(MyData$V1, MyData$V2, 0.05)
