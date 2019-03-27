library(TeachingDemos) 

KolmogorovSmirnovTest <- function(data, mean, std, alpha) {
  x <- ks.test(data, "pnorm", mean, std, alternative = "two.sided")
  print(sprintf("KS-test stat: %.4f", x$statistic))
  if (x$p.value > alpha) {
    print("Nezamítáme H0")
  } else {
    print("Zamítáme H0")
  }
  
}
