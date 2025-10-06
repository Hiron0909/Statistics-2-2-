# Lab 12: Derive the Best Critical Region for Simple vs. Composite Hypotheses
# Parameters
mu0 <- 5 # Null hypothesis mean
mu1 <- 6 # Alternative hypothesis mean
sigma <- 2 # Population standard deviation
n <- 30 # Sample size
alpha <- 0.05 # Significance level
# Generate sample data under H0
set.seed(123)
sample_data_H0 <- rnorm(n, mean = mu0, sd = sigma)
# Generate sample data under H1
sample_data_H1 <- rnorm(n, mean = mu1, sd = sigma)
# Likelihood ratio test
likelihood_ratio <- function(data, mu0, mu1, sigma) {
 exp(sum(dnorm(data, mean = mu1, sd = sigma, log = TRUE)) -
 sum(dnorm(data, mean = mu0, sd = sigma, log = TRUE)))
}
# Critical region
critical_value <- qnorm(1 - alpha, mean = mu0, sd = sigma / sqrt(n))
# Decision
decision_H0 <- mean(sample_data_H0) > critical_value
decision_H1 <- mean(sample_data_H1) > critical_value
# Output
print(paste("Critical Value:", critical_value))
print(paste("Decision under H0:", decision_H0))
print(paste("Decision under H1:", decision_H1))
# Graphical Output
par(mfrow = c(1, 2))
# Density plot under H0
plot(density(sample_data_H0), col = "blue", lwd = 2, main = "Density under H0", xlab = "Value")
abline(v = critical_value, col = "red", lty = 2)
# Density plot under H1
plot(density(sample_data_H1), col = "green", lwd = 2, main = "Density under H1", xlab = "Value")
abline(v = critical_value, col = "red", lty = 2)
