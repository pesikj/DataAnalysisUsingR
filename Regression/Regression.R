MyData <- read.csv2(file = "data.csv", header = FALSE, sep = ";")

alpha1 = 0.90;
alpha2 = 0.90;
xest = 2.9;

model = data.frame (y = MyData$V2, x = MyData$V1)
fit <- lm(y ~ x, data = model)
summary(fit)
intervalestcoef <- confint(fit, level=alpha1)
intervalpreditexpval <- predict(fit, newdata=data.frame(x=c(xest)), interval="confidence", level=alpha2)
intervalpredittermval <- predict(fit, newdata=data.frame(x=c(xest)), interval="prediction", level=alpha2)

print(paste0("Regression equation: y = ", fit$coefficients["(Intercept)"], " + ", fit$coefficients["x"], " * x" ))
print(paste0("Regression - index determinace: ", summary(fit)$r.squared))
print(paste0("Regression - korelační koeficient: ", sqrt(summary(fit)$r.squared)))
print(paste0("Regression - interval est. b_0: ", intervalestcoef[1,1], " , ",  intervalestcoef[1,2]))
print(paste0("Regression - interval est. b_1: ", intervalestcoef[2,1], " , ",  intervalestcoef[2,2]))
print(paste0("Regression - interval est. stredni hodnota y ", intervalpreditexpval[2], " , ",  intervalpreditexpval[3]))
#print(paste0("Regression - interval est. individualni hodnota y ", intervalpredittermval[2], " , ",  intervalpredittermval[3]))
