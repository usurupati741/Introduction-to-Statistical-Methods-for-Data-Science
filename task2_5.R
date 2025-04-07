# Helper function to get residuals
get_residuals <- function(X, y, theta_hat) {
  y_hat <- X %*% theta_hat
  residuals <- y - y_hat
  return(residuals)
}

# Get residuals for each model
res_1 <- get_residuals(X1, y, theta_1)
res_2 <- get_residuals(X2, y, theta_2)
res_3 <- get_residuals(X3, y, theta_3)
res_4 <- get_residuals(X4, y, theta_4)
res_5 <- get_residuals(X5, y, theta_5)

# Combine into a long-format data frame for ggplot
residual_data <- data.frame(
  Residual = c(res_1, res_2, res_3, res_4, res_5),
  Model = factor(rep(c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
                     each = length(res_1)))
)
library(ggplot2)

# Histogram plots
ggplot(residual_data, aes(x = Residual)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  facet_wrap(~Model, scales = "free") +
  ggtitle("Residual Histograms for Each Model")

# Q-Q plots
ggplot(residual_data, aes(sample = Residual)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Model, scales = "free") +
  ggtitle("Q-Q Plots of Residuals for Each Model")
