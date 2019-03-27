MyData <- read.csv2(file = "data.csv", header = FALSE, sep = ";")
TwoSamleTTest(MyData$V1, MyData$V2, "equal", alpha, FALSE, TRUE)

TwoSamleTTest <- function(sample1, sample2, alternative, alpha, pairedSamples, equalVariance) {
  x <- t.test(sample1, sample2, var.equal = equalVariance, paired = pairedSamples)
  if (alternative == "equal") {
    print(paste0("T-test conf: ", qt(alpha/2,x$parameter), " ", qt(1-alpha/2,x$parameter)))
  } else if (alternative == "greater") {
    print(paste0("T-test conf: ", qt(1-alpha,x$parameter), " infty"))
  } else if (alternative == "less") {
    print(paste0("T-test conf: -infty, ", qt(alpha,x$parameter)))
  }
  print(paste0("T-test stat: ", x$statistic))
  print(paste0("T-test p-value: ", x$p.value))
  if (x$p.value > alpha) {
    print("Nezamítáme H0")
  } else {
    print("Zamítáme H0")
  }
}
