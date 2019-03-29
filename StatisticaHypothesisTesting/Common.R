PrintResult <- function(pvalue, alpha){
  if (pvalue > alpha) {
    print("Nezamítáme H0")
  } else {
    print("Zamítáme H0")
  }
}

