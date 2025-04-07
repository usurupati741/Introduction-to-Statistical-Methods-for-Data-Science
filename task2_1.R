# Load data
x <- read.csv("X.csv")
y <- read.csv("y.csv")
time <- read.csv("time.csv")

# Combine all into one data frame
data <- cbind(time, x, y)
colnames(data) <- c("time", "x1", "x2", "y")

# Convert x2 to numeric just to be safe
data$x2 <- as.numeric(data$x2)

# Helper function to estimate θ using least squares
estimate_parameters <- function(X, y) {
  theta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  return(theta_hat)
}

# Prepare input (x1, x2) and output (y)
x1 <- data$x1
x2 <- data$x2
y <- data$y

# Create design matrices for each model
# Model 1: y = θ1*x1^3 + θ2*x1^5 + θ3*x2 + θbias
X1 <- cbind(x1^3, x1^5, x2, 1)

# Model 2: y = θ1*x1 + θ2*x2 + θbias
X2 <- cbind(x1, x2, 1)

# Model 3: y = θ1*x1 + θ2*x1^2 + θ3*x1^4 + θ4*x2 + θbias
X3 <- cbind(x1, x1^2, x1^4, x2, 1)

# Model 4: y = θ1*x1 + θ2*x1^2 + θ3*x1^3 + θ4*x1^5 + θ5*x2 + θbias
X4 <- cbind(x1, x1^2, x1^3, x1^5, x2, 1)

# Model 5: y = θ1*x1 + θ2*x1^3 + θ3*x1^4 + θ4*x2 + θbias
X5 <- cbind(x1, x1^3, x1^4, x2, 1)

# Estimate θ for each model
theta_1 <- estimate_parameters(X1, y)
theta_2 <- estimate_parameters(X2, y)
theta_3 <- estimate_parameters(X3, y)
theta_4 <- estimate_parameters(X4, y)
theta_5 <- estimate_parameters(X5, y)

# Display results
cat("Model 1 θ:\n"); print(theta_1)
cat("Model 2 θ:\n"); print(theta_2)
cat("Model 3 θ:\n"); print(theta_3)
cat("Model 4 θ:\n"); print(theta_4)
cat("Model 5 θ:\n"); print(theta_5)
