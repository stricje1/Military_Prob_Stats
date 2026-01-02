library(knitr)
knitr::opts_chunk$set(echo=TRUE, message=FALSE, fig.path='figures/', dpi=300, digits = 2)
options(digits = 5, scipen = 100)


# Set the random seed for reproducibility
set.seed(42)

# Generate a non-normally distributed population
population <- runif(5000, min = 0, max = 1)

# Create a histogram of the population
par(mfrow = c(1, 2))  # Set up a 1x2 grid for plotting

# Plot the histogram of the population
hist(population, breaks = 30, prob = TRUE, main = "Population Distribution",
     xlab = "Value", col = "lightblue")

# Step 2 and 3: Draw random samples and calculate sample means
sample_size <- 30
num_samples <- 500

# Empty vector to store sample means
sample_means <- c()

for (i in 1:num_samples) {
  # Take a random sample
  sample <- sample(population, size = sample_size, replace = TRUE)
  
  # Calculate the mean of the sample
  sample_means[i] <- mean(sample)
}

# For sample
x_bar <- mean(sample_means)
std <- sd(sample_means)

print('Sample Mean and Variance')
print(x_bar)
print(std**2)

# For Population
mu <-mean(population)
sigma <- sd(population)

print('Population Mean and Variance')
print(mu)
print((sigma**2)/sample_size)

# Plot the histogram of sample means
hist(sample_means, breaks = 30, prob = TRUE, main = "Distribution of Sample Means",
     xlab = "Sample Mean", col = "lightgreen")

# Overlay density curves
curve(dnorm(x, mean = x_bar, sd = std), col = "black", lwd = 2, add = TRUE)

# Add labels and legends
legend("topright", legend = c("Distribution Curve"),
       col = c("black"), lwd = 2)

# Reset the plot layout
par(mfrow = c(1, 1))

