MyData <- read.csv2(file = "data.csv", header = FALSE, sep = ";")

Regression <- function(x, y, alpha1, alpha2, xest) {
  model = data.frame (y = y, x = x)
  fit <- lm(y ~ x, data = model)
  summary(fit)
  intervalestcoef <- confint(fit, level=alpha1)
  intervalpreditexpval <- predict(fit, newdata=data.frame(x=c(xest)), interval="confidence", level=alpha2)
  intervalpredittermval <- predict(fit, newdata=data.frame(x=c(xest)), interval="prediction", level=alpha2)
  
  print(sprintf("Regression equation: y = %.4f + %.4f x", fit$coefficients["(Intercept)"], fit$coefficients["x"]))
  print(sprintf("Regression - index determinace: %.4f", summary(fit)$r.squared))
  print(sprintf("Regression - korelační koeficient: %.4f", sqrt(summary(fit)$r.squared)))
  print(sprintf("Regression - interval est. b_0: <%.4f, %.4f>", intervalestcoef[1,1], intervalestcoef[1,2]))
  print(sprintf("Regression - interval est. b_1: <%.4f, %.4f>", intervalestcoef[2,1], intervalestcoef[2,2]))
  print(sprintf("Regression - interval est. pro x = %.4f: y in <%.4f, %.4f>", xest, intervalpreditexpval[2], intervalpreditexpval[3]))
  #print(paste0("Regression - interval est. individualni hodnota y ", intervalpredittermval[2], " , ",  intervalpredittermval[3]))
}