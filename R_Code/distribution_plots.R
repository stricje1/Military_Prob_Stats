library(knitr)
knitr::opts_chunk$set(echo=TRUE, message=FALSE, fig.path='figures/', dpi=300, digits = 2)
options(digits = 5, scipen = 100)

# import Rlab library 
library(Rlab) 
set.seed(9999) 
# sample size 
N <- 100 
# generate random variables using 
# rbern( ) function 
random_values <- rbern(N, prob = 0.5) 
# print the values 
print(random_values) 
# plot of randomly 
# drawn density 
hist(random_values,breaks = 10,xlab = "", ylab = "", main = "Bernoulli")

set.seed(3223)
N <- 10000
random_values <- runif(N, min=0, max=1) 
hist(random_values,breaks = 10,xlab = "", ylab = "", main = "Uniform")

set.seed(3223)
N <- 1000
#random_values <- dbinom(3, size = 13, prob = 1/6)
probabilities = dbinom(x = c(0:10), size = 13, prob = 1/6)
random_values <- data.frame(probabilities)
random_values
hist(random_values, breaks = 10, xlab = "", ylab = "", main = "Binomial")

x<-dbinom(0:25,size=10,prob=0.5)
plot(0:25,x,type="h")

# set trials to 10
trials <- 20
# set probability of success
p <- 0.35
successes <- seq(from = 0, to = trials, by = 1)
binomial.prob <- dbinom(x = successes, size = trials, prob = p)
barplot(binomial.prob,
        main = paste0("Binomial"),
        xlab = "",
        names.arg = successes,
        ylab = "",
        ylim = c(0,0.2),
        col = "grey")


# Load necessary libraries
library(ggplot2)

# Parameters
p <- 0.3
k_values <- 1:20

# Calculate PMF
pmf_values <- dgeom(k_values - 1, prob = p)
pmf = pmf_values
# Create a data frame for plotting
pmf_data <- data.frame(k = k_values, pmf)

# Plot the PMF
ggplot(pmf_data, aes(x = k, y = pmf)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "Geometric",
       x = "",
       y = "") +
  theme_minimal()

barplot(pmf,
        main = paste0("Binomial"),
        xlab = "",
        names.arg = pmf_values,
        ylab = "",
        ylim = c(0,0.3),
        col = "grey")

# Declaring the data points 
# from 1 to 100
data_points <-1:20

# Specifying the lambda value
val <- 4


# Plotting the data points
plot(dpois(x= data_points,lambda= val),type="b",
     xlab = "",
     ylab = "Poisson")

pmf <- dpois(x= data_points,lambda= val)
barplot(pmf,
        main = paste0("Poisson"),
        xlab = "",
        names.arg = pmf,
        ylab = "",
        ylim = c(0,0.2),
        col = "grey")

pmf <- dexp(x, rate = .5)
barplot(pmf,
        main = paste0("Exponential"),
        xlab = "",
         ylab = "",
        ylim = c(0,.3),
        col = "grey")

hist(pmf,breaks = 50,xlab = "", ylab = "", main = "Exponential")
x <-1:20
curve(dexp(x, rate = .5), from=0, to=10, col='blue')

x <-1:15
pmf<- dweibull(x, shape=4, scale=3)
barplot(pmf,
        main = paste0("Weibull"),
        xlab = "",
        ylab = "",
        ylim = c(0,.5),
        col = "grey")
curve(dweibull(x, shape=4, scale = 3), from=0, to=6)

p <- 0.3
k_values <- 1:15

# Calculate PMF
pmf_values<- dweibull(k_values, shape=4, scale=3)
pmf = pmf_values
# Create a data frame for plotting
pmf_data <- data.frame(k = k_values, pmf)

ggplot(pmf_data, aes(x = k_values, y = pmf)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "Weibull",
       x = "",
       y = "") +
  theme_minimal()



cylinders <- read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\cylinders.csv")
summary(cylinders)
