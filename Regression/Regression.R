MyData <- read.csv2(file = "data.csv", header = FALSE, sep = ";")

Regression <- function(x, y, alpha1, alpha2, xest) {
  model = data.frame (y = y, x = x)
  fit <- lm(y ~ x, data = model)
  summary(fit)
  intervalestcoef <- confint(fit, level=alpha1)
  intervalpreditexpval <- predict(fit, newdata=data.frame(x=c(xest)), interval="confidence", level=alpha2)
  intervalpredittermval <- predict(fit, newdata=data.frame(x=c(xest)), interval="prediction", level=alpha2)
  
  print(sprintf("Regression equation: y = %.5f + %.5f x", fit$coefficients["(Intercept)"], fit$coefficients["x"]))
  print(sprintf("Regression - index determinace: %.5f", summary(fit)$r.squared))
  print(sprintf("Regression - korelační koeficient: %.5f", sqrt(summary(fit)$r.squared)))
  print(sprintf("Regression - interval est. b_0: <%.5f, %.5f>", intervalestcoef[1,1], intervalestcoef[1,2]))
  print(sprintf("Regression - interval est. b_1: <%.5f, %.5f>", intervalestcoef[2,1], intervalestcoef[2,2]))
  print(sprintf("Regression - interval est. pro x = %.5f: y in <%.5f, %.5f>", xest, intervalpreditexpval[2], intervalpreditexpval[3]))
  #print(paste0("Regression - interval est. individualni hodnota y ", intervalpredittermval[2], " , ",  intervalpredittermval[3]))
}

