MyData <- read.csv2(file = "data.csv", header = FALSE, sep = ";")

# Two sample t-test, two sided, equal variances
alpha = 0.05;
x <- t.test(MyData$V1, MyData$V2, var.equal = TRUE)
print(paste0("T-test p-value: ", x$p.value))
print(paste0("T-test stat: ", x$statistic))
print(paste0("T-test conf: ", qt(alpha/2,x$parameter), " ", qt(1-alpha/2,x$parameter)))

# Two sample t-test, two sided, greater
alpha = 0.05;
x <- t.test(MyData$V1, MyData$V2, var.equal = TRUE, alternative = "greater")
print(paste0("T-test p-value: ", x$p.value))
print(paste0("T-test stat: ", x$statistic))
print(paste0("T-test conf: ", qt(1-alpha,x$parameter), " infty"))

# Two sample t-test, two sided, less
alpha = 0.05;
x <- t.test(MyData$V1, MyData$V2, var.equal = TRUE, alternative = "less")
print(paste0("T-test p-value: ", x$p.value))
print(paste0("T-test stat: ", x$statistic))
print(paste0("T-test conf: -infty, ", qt(alpha,x$parameter)))