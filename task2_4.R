# Helper function to compute AIC and BIC
compute_aic_bic <- function(log_likelihood, k, n) {
  aic <- 2 * k - 2 * log_likelihood
  bic <- k * log(n) - 2 * log_likelihood
  return(c(AIC = aic, BIC = bic))
}

# Number of parameters (including bias term) in each model
k1 <- 4  # x1^3, x1^5, x2, bias
k2 <- 3  # x1, x2, bias
k3 <- 5  # x1, x1^2, x1^4, x2, bias
k4 <- 6  # x1, x1^2, x1^3, x1^5, x2, bias
k5 <- 5  # x1, x1^3, x1^4, x2, bias

# Compute AIC and BIC for each model
info_1 <- compute_aic_bic(ll_1, k1, n)
info_2 <- compute_aic_bic(ll_2, k2, n)
info_3 <- compute_aic_bic(ll_3, k3, n)
info_4 <- compute_aic_bic(ll_4, k4, n)
info_5 <- compute_aic_bic(ll_5, k5, n)

# Display results
cat("\nModel AIC & BIC values:\n")
cat("Model 1: AIC =", info_1["AIC"], ", BIC =", info_1["BIC"], "\n")
cat("Model 2: AIC =", info_2["AIC"], ", BIC =", info_2["BIC"], "\n")
cat("Model 3: AIC =", info_3["AIC"], ", BIC =", info_3["BIC"], "\n")
cat("Model 4: AIC =", info_4["AIC"], ", BIC =", info_4["BIC"], "\n")
cat("Model 5: AIC =", info_5["AIC"], ", BIC =", info_5["BIC"], "\n")
