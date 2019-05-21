TwoSamleTTest <- function(sample1, sample2, alternative, alpha, pairedSamples, equalVariance) {
  x <- t.test(sample1, sample2, var.equal = equalVariance, paired = pairedSamples)
  if (pairedSamples == FALSE && equalVariance == TRUE) {
    if (alternative == "two.sided") {
      print(sprintf("T-test conf: (-nekonecno, %.5f> U <%.5f, nekonecno)", qt(alpha/2,x$parameter), qt(1-alpha/2,x$parameter)))
    } else if (alternative == "greater") {
      print(sprintf("T-test conf: <%.5f, nekonecno)", qt(1-alpha,x$parameter)))
    } else if (alternative == "less") {
      print(sprintf("T-test conf: (-nekonecno, %.5f>", qt(alpha,x$parameter)))
    }
  } else if (pairedSamples == FALSE && equalVariance == FALSE) {
    if (alternative == "two.sided") {
      print(sprintf("T-test conf: (-nekonecno, %.5f> U <%.5f, nekonecno)", qt(alpha/2,round(x$parameter)), qt(1-alpha/2,round(x$parameter))))
    } else if (alternative == "greater") {
      print(sprintf("T-test conf: <%.5f, nekonecno)", qt(1-alpha,round(x$parameter))))
    } else if (alternative == "less") {
      print(sprintf("T-test conf: (-nekonecno, %.5f>", qt(alpha,round(x$parameter))))
    }
  } else if (pairedSamples == TRUE && equalVariance == TRUE) {
    if (alternative == "two.sided") {
      print(sprintf("T-test conf: (-nekonecno, %.5f> U <%.5f, nekonecno)", qt(alpha/2,x$parameter), qt(1-alpha/2,x$parameter)))
    } else if (alternative == "greater") {
      print(sprintf("T-test conf: <%.5f, nekonecno)", qt(1-alpha,x$parameter)))
    } else if (alternative == "less") {
      print(sprintf("T-test conf: (-nekonecno, %.5f>", qt(alpha,x$parameter)))
    }
  }
  print(sprintf("T-test stat: %.5f", x$statistic))
  print(sprintf("T-test p-value: %.5f", x$p.value))
  PrintResult(x$p.value, alpha)
}


TwoSampleFTest <- function(sample1, sample2, alternative, alpha) {
  x <- var.test(sample1, sample2, alternative = alternative)
  if (alternative == "two.sided") {
    print(sprintf("Variance test conf: <0, %.5f> U <%.5f, nekonecno)", 
               qf(alpha / 2, x$parameter[1], x$parameter[2]), qf(1 - alpha / 2, x$parameter[1], x$parameter[2])))
  }
  print(sprintf("Variance test stat: %.5f", x$statistic))
  print(sprintf("Variance test p-value: %.5f", x$p.value))
  PrintResult(x$p.value, alpha)
}


IndependenceTest <- function(sample1, sample2, alpha) {
  tbl = table(sample1, sample2)
  x = chisq.test(tbl)
  print(sprintf("Independence test stat: %.5f", x$statistic))
  print(sprintf("Independence test p-value: %.5f", x$p.value))
  PrintResult(x$p.value, alpha)
}