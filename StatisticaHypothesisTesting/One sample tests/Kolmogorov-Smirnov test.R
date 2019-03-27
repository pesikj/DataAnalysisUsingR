library(TeachingDemos) 

KolmogorovSmirnovTest <- function(data, mean, std, alpha) {
  x <- ks.test(data,  "pnorm", mean, std, alternative = "two.sided")
  print(paste0("KS-test stat: ", x$statistic))
  if (x$p.value > alpha) {
    print("Nezamítáme H0")
  } else {
    print("Zamítáme H0")
  }
  
}
