# Load necessary library
library(BSDA)

# Set seed for reproducibility
set.seed(123)

# Parameters
mu <- 5
sigma <- 2
n <- 30
N_sim <- 1000

# Simulation using replicate()
samples <- replicate(N_sim, rnorm(n, mean = mu, sd = sigma))
sample_vars <- apply(samples, 2, var)  # Unbiased variance
biased_vars <- colSums((samples - rowMeans(samples))^2) / n  # Biased variance

# Function for plotting histograms
plot_hist <- function(data, title, color) {
  hist(data, breaks = 30, col = color, main = title, xlab = "Variance", border = "white")
  abline(v = sigma^2, col = "red", lwd = 2)
}

# Graphical Output
par(mfrow = c(1, 2))
plot_hist(sample_vars, "Unbiased Sample Variance", "lightblue")
plot_hist(biased_vars, "Biased Sample Variance", "lightgreen")

# Generate groups for z-test
group1 <- rnorm(30, mean = 50, sd = 10)
group2 <- rnorm(30, mean = 55, sd = 10)

# Perform z-test
z_test_result <- z.test(group1, group2, sigma.x = sd(group1), sigma.y = sd(group2))

# Output test result
cat("Z-Test Results:\n")
print(z_test_result)

# Graphical Output for groups
par(mfrow = c(1, 2))
plot_hist(group1, "Histogram of Group 1", "lightblue")
plot_hist(group2, "Histogram of Group 2", "lightgreen")
