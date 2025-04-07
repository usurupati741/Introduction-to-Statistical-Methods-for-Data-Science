# Load required libraries
library(ggplot2)
library(gridExtra)

# Read the data
x_data <- read.csv("X.csv", header = TRUE)
y <- read.csv("y.csv", header = TRUE)$y
x1 <- x_data$x1
x2 <- x_data$x2

# Prepare dataframe
df <- data.frame(
  y = y,
  x1 = x1,
  x2 = x2,
  x1_2 = x1^2,
  x1_4 = x1^4
)

# Fit Model 3: y ~ x1 + x1^2 + x1^4 + x2
model3 <- lm(y ~ x1 + x1_2 + x1_4 + x2, data = df)
summary(model3)

# Get estimated parameters
theta_hat <- coef(model3)
print(theta_hat)

# Select top 2 parameters with largest absolute values
theta_sorted <- sort(abs(theta_hat), decreasing = TRUE)
top_names <- names(theta_sorted)[1:2]
top_names

# Fix all other parameters
fixed_params <- theta_hat[!(names(theta_hat) %in% top_names)]

# Simulate y based on current sampled parameters
simulate_y <- function(sampled_params) {
  # Create a prediction using fixed and sampled parameters
  beta <- theta_hat
  beta[top_names[1]] <- sampled_params[1]
  beta[top_names[2]] <- sampled_params[2]
  
  y_sim <- beta["(Intercept)"] +
    beta["x1"] * df$x1 +
    beta["x1_2"] * df$x1_2 +
    beta["x1_4"] * df$x1_4 +
    beta["x2"] * df$x2
  return(y_sim)
}

# Rejection ABC
set.seed(42)
n_samples <- 10000
eps <- quantile((y - predict(model3))^2, 0.01)  # tolerance based on low SSE
accepted <- list()
priors <- list()

for (param in top_names) {
  center <- theta_hat[param]
  priors[[param]] <- runif(n_samples, min = center - 2, max = center + 2)
}

distances <- numeric(n_samples)

for (i in 1:n_samples) {
  sampled_params <- c(priors[[top_names[1]]][i], priors[[top_names[2]]][i])
  y_sim <- simulate_y(sampled_params)
  distances[i] <- sum((y - y_sim)^2)
}

# Accept samples with small distance
threshold <- quantile(distances, 0.01)
accepted_indices <- which(distances <= threshold)

# Posterior samples
posterior_1 <- priors[[top_names[1]]][accepted_indices]
posterior_2 <- priors[[top_names[2]]][accepted_indices]

# Plot marginal distributions
p1 <- ggplot(data.frame(posterior_1), aes(x = posterior_1)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = paste("Posterior of", top_names[1]), x = top_names[1])

p2 <- ggplot(data.frame(posterior_2), aes(x = posterior_2)) +
  geom_histogram(bins = 30, fill = "salmon", color = "black") +
  labs(title = paste("Posterior of", top_names[2]), x = top_names[2])

# Joint plot
joint_df <- data.frame(p1 = posterior_1, p2 = posterior_2)
p_joint <- ggplot(joint_df, aes(x = p1, y = p2)) +
  geom_point(alpha = 0.5) +
  labs(title = "Joint Posterior", x = top_names[1], y = top_names[2]) +
  theme_minimal()

# Arrange plots
grid.arrange(p1, p2, p_joint, nrow = 2)
