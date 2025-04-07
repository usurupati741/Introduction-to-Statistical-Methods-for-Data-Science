# Load required libraries
library(tidyverse)
library(ggplot2)
install.packages("GGally")

library(GGally)

# Read the data
x <- read.csv("X.csv")
y <- read.csv("y.csv")
time <- read.csv("time.csv")

# Combine into one dataframe
data <- cbind(time, x, y)
colnames(data) <- c("time", "x1", "x2", "y")

# Convert x2 to factor for plotting
data$x2 <- factor(data$x2, levels = c(0,1), labels = c("Neutral", "Emotional"))

# ---- 1. Time Series Plots ----

# Plot input sound signal (x1) and output MEG signal (y)
ggplot(data, aes(x = time)) +
  geom_line(aes(y = x1), color = "blue") +
  labs(title = "Input Sound Signal (x1) over Time", y = "x1", x = "Time (s)")

ggplot(data, aes(x = time)) +
  geom_line(aes(y = y), color = "red") +
  labs(title = "Output MEG Signal (y) over Time", y = "y", x = "Time (s)")

# ---- 2. Distribution Plots ----

# Histogram/density of x1 and y
ggplot(data, aes(x = x1)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.6) +
  labs(title = "Distribution of Input Signal (x1)")

ggplot(data, aes(x = y)) +
  geom_histogram(bins = 30, fill = "red", alpha = 0.6) +
  labs(title = "Distribution of Output MEG Signal (y)")

# ---- 3. Correlation and Scatter Plots ----

# Scatter plot between x1 and y
ggplot(data, aes(x = x1, y = y)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  labs(title = "Scatter Plot: x1 vs y", x = "Input Signal (x1)", y = "MEG Signal (y)")

# Correlation
correlation <- cor(data$x1, data$y)
print(paste("Correlation between x1 and y:", round(correlation, 3)))

# ---- 4. Boxplots of Output Signal by Category ----

ggplot(data, aes(x = x2, y = y, fill = x2)) +
  geom_boxplot() +
  labs(title = "MEG Signal by Audio Category", x = "Audio Type", y = "MEG Signal (y)")

# ---- 5. Separate Analysis for x2 = 0 (Neutral) and x2 = 1 (Emotional) ----

# Split data
neutral_data <- filter(data, x2 == "Neutral")
emotional_data <- filter(data, x2 == "Emotional")

# Scatter plots for each category
ggplot(neutral_data, aes(x = x1, y = y)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "x1 vs y (Neutral Voice)", x = "x1", y = "y")

ggplot(emotional_data, aes(x = x1, y = y)) +
  geom_point(alpha = 0.5, color = "red") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "x1 vs y (Emotional Voice)", x = "x1", y = "y")
