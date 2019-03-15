library(TeachingDemos) 
MyData <- read.csv2(file = "data.csv", header = FALSE, sep = ";", encoding = "UTF-8")

x <- ks.test(MyData$V1,  "pnorm", 61, 9, alternative = "two.sided")
print(paste0("KS-test p-value: ", x$p.value))
print(paste0("KS-test stat: ", x$statistic))
