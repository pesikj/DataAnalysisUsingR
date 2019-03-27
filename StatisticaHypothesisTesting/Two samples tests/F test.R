library(TeachingDemos)
MyData <- read.csv2(file = "data.csv", header = FALSE, sep = ";")
TwoSampleFTest(MyData$V1, MyData$V2, "two.sided", 0.05)

TwoSampleFTest <- function(sample1, sample2, alternative, alpha) {
  x <- var.test(sample1, sample2, alternative = alternative)
  print(paste0("Variance test p-value: ", x$p.value))
  print(paste0("Variance test stat: ", x$statistic))
  print(paste0("Variance test conf: 0, ", qf(alpha / 2, x$parameter[1], x$parameter[2]), " ", qf(1 - alpha / 2, x$parameter[1], x$parameter[2]), " infty"))
}