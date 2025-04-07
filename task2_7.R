set.seed(123)  # For reproducibility

# --- Step 1: Split into training/testing (70/30) ---
n <- nrow(data)
train_index <- sample(1:n, size = 0.7 * n)
test_index <- setdiff(1:n, train_index)

# Training and testing sets
X4_train <- X4[train_index, ]
y_train <- y[train_index]

X4_test <- X4[test_index, ]
y_test <- y[test_index]

# --- Step 2: Estimate model parameters using training data ---
theta_4_train <- solve(t(X4_train) %*% X4_train) %*% t(X4_train) %*% y_train

# --- Step 3: Predict on testing data ---
y_pred <- X4_test %*% theta_4_train

# --- Step 4: Compute residual variance and 95% confidence intervals ---
residuals_train <- y_train - X4_train %*% theta_4_train
sigma2_hat <- sum(residuals_train^2) / (length(y_train) - ncol(X4_train))  # residual variance

# Compute standard errors of predictions
pred_var <- diag(X4_test %*% solve(t(X4_train) %*% X4_train) %*% t(X4_test)) * sigma2_hat
pred_se <- sqrt(pred_var)

# 95% confidence intervals
lower_ci <- y_pred - 1.96 * pred_se
upper_ci <- y_pred + 1.96 * pred_se

# --- Step 5: Plot predictions and confidence intervals ---
# Combine data for plotting
plot_df <- data.frame(
  Index = 1:length(y_test),
  Prediction = as.numeric(y_pred),
  Actual = as.numeric(y_test),
  Lower = as.numeric(lower_ci),
  Upper = as.numeric(upper_ci)
)

library(ggplot2)

ggplot(plot_df, aes(x = Index)) +
  geom_line(aes(y = Actual), color = "blue", size = 1, alpha = 0.6) +
  geom_line(aes(y = Prediction), color = "red", size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, fill = "orange") +
  ggtitle("Model 4 Predictions vs Actual (Testing Set)") +
  ylab("MEG Signal") +
  xlab("Sample Index") +
  theme_minimal()
