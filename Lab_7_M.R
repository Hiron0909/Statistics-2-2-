mu <- 5; sigma <- 2; ns <- c(10, 30, 100, 500, 1000); N <- 1000
set.seed(123)
res <- sapply(ns, function(n) {
  sm <- replicate(N, mean(rnorm(n, mu, sigma)))
  c(mean(sm), var(sm))
})
par(mfrow = c(1, 2))
plot(ns, res[1, ], type = "b", col = "blue", main = "Convergence of Sample Mean", xlab = "Sample Size", ylab = "Sample Mean"); abline(h = mu, col = "red", lwd = 2)
plot(ns, res[2, ], type = "b", col = "green", main = "Convergence of Sample Variance", xlab = "Sample Size", ylab = "Sample Variance"); abline(h = sigma^2, col = "red", lwd = 2)