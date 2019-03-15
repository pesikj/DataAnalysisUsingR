# Sign test, two samples, greater
alpha = 0.1;
stat = sum(MyData$V1 - MyData$V2 < 0);
n = colSums(!is.na(MyData));
x <- binom.test(stat, n["V1"], alternative = "greater");
print(paste0("Sign test p-value: ", x$p.value))
defoborstat <- data.frame(0:n["V1"], pbinom(rep(n,n+1) - 0:n["V1"], n["V1"], 0.5))
colnames(defoborstat) <- c("X", "PX")
krioborright <- defoborstat[defoborstat$PX <= alpha,]
print(paste0("Sign test stat: ", stat))
print(paste0("Sign test krit: ", head(krioborright$X,n=1L), ", .. , ", n ))