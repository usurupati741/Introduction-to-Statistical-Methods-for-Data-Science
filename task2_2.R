# Helper function to compute RSS
compute_rss <- function(X, y, theta_hat) {
  y_hat <- X %*% theta_hat
  residuals <- y - y_hat
  rss <- sum(residuals^2)
  return(rss)
}

# Compute RSS for each model
rss_1 <- compute_rss(X1, y, theta_1)
rss_2 <- compute_rss(X2, y, theta_2)
rss_3 <- compute_rss(X3, y, theta_3)
rss_4 <- compute_rss(X4, y, theta_4)
rss_5 <- compute_rss(X5, y, theta_5)

# Display RSS values
cat("\nModel RSS values:\n")
cat("Model 1 RSS:", rss_1, "\n")
cat("Model 2 RSS:", rss_2, "\n")
cat("Model 3 RSS:", rss_3, "\n")
cat("Model 4 RSS:", rss_4, "\n")
cat("Model 5 RSS:", rss_5, "\n")
