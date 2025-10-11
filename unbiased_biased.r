set.seed(123)

# Parameters
mu <- 5; sigma <- 2; n <- 30; N <- 1000

# Simulate samples
samples <- replicate(N, rnorm(n, mu, sigma))

# Compute variances
v_unbiased <- apply(samples, 2, var)
v_biased <- apply(samples, 2, function(x) sum((x - mean(x))^2)/n)

# Plot histograms
par(mfrow=c(1,2))
hist(v_unbiased, breaks=30, col="lightblue", main="Unbiased Variance", xlab="Variance"); abline(v=sigma^2, col="red", lwd=2)
hist(v_biased, breaks=30, col="lightgreen", main="Biased Variance", xlab="Variance"); abline(v=sigma^2, col="red", lwd=2)

# Z-test for two groups
g1 <- rnorm(30, 50, 10); g2 <- rnorm(30, 55, 10)
z <- (mean(g1)-mean(g2)) / sqrt(sd(g1)^2/length(g1) + sd(g2)^2/length(g2))
p <- 2*(1 - pnorm(abs(z)))
cat("Z =", round(z,3), "p-value =", round(p,4), "\n")

# Plot group histograms
par(mfrow=c(1,2))
hist(g1, breaks=15, col="lightblue", main="Group 1", xlab="Value")
hist(g2, breaks=15, col="lightgreen", main="Group 2", xlab="Value")
