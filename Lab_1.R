# Lab 1: Verification of Fisher’s Lemma (Independence of Mean & Variance)

set.seed(123)
mu <- 5; sigma <- 2; n <- 30; N_sim <- 1000

res <- replicate(N_sim, {
  x <- rnorm(n, mu, sigma)
  c(mean(x), var(x))
})
sample_means <- res[1, ]
sample_vars  <- res[2, ]

cat("Correlation between sample mean and variance:", cor(sample_means, sample_vars), "\n")

par(mfrow = c(2,2))

hist(sample_means, col="lightblue", main="Sample Means", xlab="Mean", border="white")
curve(dnorm(x, mu, sigma/sqrt(n)), add=TRUE, col="red", lwd=2)

hist(sample_vars, col="lightgreen", main="Sample Variances", xlab="Variance", border="white")

plot(sample_means, sample_vars, pch=19, col=rgb(0,0,1,0.5),
     main="Mean vs Variance", xlab="Mean", ylab="Variance")
abline(h=sigma^2, col="red", lwd=2)

qqnorm(sample_means, col="blue", main="Q-Q Plot (Means)")
qqline(sample_means, col="red", lwd=2)
