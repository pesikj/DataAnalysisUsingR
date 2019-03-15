library(TeachingDemos)
MyData <- read.csv2(file = "data.csv", header = FALSE, sep = ";")
alpha = 0.05;
n1 <- length(which(!is.na(MyData$V1)));
n2 <- length(which(!is.na(MyData$V2)));

# One sample sigma test, one sided, greater
x <- var.test(MyData$V1, MyData$V2)
print(paste0("Variance test p-value: ", x$p.value))
print(paste0("Variance test stat: ", x$statistic))
print(paste0("Variance test conf: 0, ", qf(alpha / 2, n1 - 1, n2 - 1), " ", qf(1 - alpha / 2, n1 - 1, n2 - 1), " infty"))