library(BSDA)
set.seed(123)
mu <- 5; sigma <- 2; n <- 30; N_sim <- 1000
samples <- replicate(N_sim, rnorm(n, mu, sigma))
v_unbiased <- sapply(1:ncol(samples), function(i) var(samples[,i]))
v_biased <- colSums((samples - mu)^2) / n
plot_v <- function(d, t, c) {
  hist(d, 30, col = c, main = t, xlab = "Variance", border = "white"); abline(v = sigma^2, col = "red", lwd = 2)
}
par(mfrow = c(1, 2))
plot_v(v_unbiased, "Unbiased vs. Biased Sample Variance", "lightblue")
plot_v(v_biased, "Biased Sample Variance", "lightgreen")
g1 <- rnorm(30, 50, 10); g2 <- rnorm(30, 55, 10)
print(z.test(g1, g2, sigma.x = sd(g1), sigma.y = sd(g2)))
par(mfrow = c(1, 2))
plot_v(g1, "Group 1 (Z-Test)", "lightblue")
plot_v(g2, "Group 2 (Z-Test)", "lightgreen")