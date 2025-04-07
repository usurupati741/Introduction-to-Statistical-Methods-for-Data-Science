# Helper function to compute log-likelihood
compute_log_likelihood <- function(rss, n) {
  sigma2_hat <- rss / (n - 1)
  log_likelihood <- - (n / 2) * log(2 * pi) - (n / 2) * log(sigma2_hat) - (1 / (2 * sigma2_hat)) * rss
  return(log_likelihood)
}

# Number of samples
n <- nrow(data)

# Compute log-likelihoods
ll_1 <- compute_log_likelihood(rss_1, n)
ll_2 <- compute_log_likelihood(rss_2, n)
ll_3 <- compute_log_likelihood(rss_3, n)
ll_4 <- compute_log_likelihood(rss_4, n)
ll_5 <- compute_log_likelihood(rss_5, n)

# Display log-likelihood values
cat("\nLog-Likelihood values:\n")
cat("Model 1 LL:", ll_1, "\n")
cat("Model 2 LL:", ll_2, "\n")
cat("Model 3 LL:", ll_3, "\n")
cat("Model 4 LL:", ll_4, "\n")
cat("Model 5 LL:", ll_5, "\n")
