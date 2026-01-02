library(probs)
A <- c(.01, .07, .19)
B <- c(.04, .12, .14, .07, .19)
union(A,B)
intersect(A,B)

n=40
nvar = 1
X<-genXdata(
  n,
  nvar = nvar,
  mu = rep(0, nvar),
  Sigma = diag(length(mu)),
  varnames = paste("x", 1:length(mu), sep = ""),
  roundto = 1
)
Y<-genXdata(
  n,
  nvar = nvar,
  mu = rep(0, nvar),
  Sigma = diag(length(mu)),
  varnames = paste("x", 1:length(mu), sep = ""),
  roundto = 1
)
union(X,Y)
intersect(X,Y)

S <- tosscoin(2, makespace = TRUE)
sims <- sim(S, ntrials = 50000)
empirical(sims)

S <- rolldie(3, makespace = TRUE)
addrv(S, sum, name = "Y")
addrv(S, Z = X3 - X2)

R <- union(X,Y)
probspace(R)

x <- seq(from = -3, to = 3, length.out = 100)
df <- data.frame(x = x, y = dnorm(x, mean = 0, sd = 1))

p <- ggplot(df, aes(x, y)) +
  geom_line() +
  labs(
    title = "Standard Normal Distribution",
    y = "Density",
    x = "Quantile"
  )
p

q75 <- quantile(df$x, .75)
q95 <- quantile(df$x, .95)

p +
  geom_ribbon(
    data = subset(df, x > q75 & x < q95),
    aes(ymax = y),
    ymin = 0,
    fill = "blue",
    colour = NA,
    alpha = 0.5
  )

q25 <- quantile(df$x, .25)
q05 <- quantile(df$x, .05)

p +
  geom_ribbon(
    data = subset(df, x > q05 & x < q25),
    aes(ymax = y),
    ymin = 0,
    fill = "blue",
    colour = NA,
    alpha = 0.5
  )

x<-c(2, 3, 4, 5, 6, 7)
p<-c(0.42, 0.23, 0.21, 0.10, 0.03, 0.01)

discrete.histogram(x,p, bar.width = 1, main="histogram for family size")


x <- seq(from = 0, to = 6, length.out = 100) # Define the density domains
ylim <- c(0, 0.6)

# Make a data.frame with densities of several distributions
df <- rbind(
  data.frame(x = x, dist_name = "Uniform"    , y = dunif(x, min   = 2, max = 4)),
  data.frame(x = x, dist_name = "Normal"     , y = dnorm(x, mean  = 3, sd = 1)),
  data.frame(x = x, dist_name = "Exponential", y = dexp(x, rate  = 1 / 2)),
  data.frame(x = x, dist_name = "Gamma"      , y = dgamma(x, shape = 2, rate = 5)) )

# Make a line plot like before, but use facet_wrap to create the grid
ggplot(data = df, aes(x = x, y = y)) +
  geom_line(size = 2, colour = "dodgerblue") +
  facet_wrap(~dist_name)   # facet and wrap by the variable dist_name

library(fitdistrplus)
Gama <- rgamma(1000, 2, 5) #+ rnorm(30, 0, .03)
fit <- fitdist(Gama, distr = "gamma", method = "mle")
summary(fit)
par(mar=c(1, 1, 1, 1))
plot(fit, lwd=2)

library("remotes")
#install_github("cran/Weighted.Desc.Stat")
library(Weighted.Desc.Stat)
library(arm)

x<-c(2, 3, 4, 5, 6, 7)
p<-c(0.42, 0.23, 0.21, 0.10, 0.03, 0.01)
w.mean(x,p)

w.sd(x,p)

discrete.histogram(x,p, bar.width = 1, main="histogram for family size")

#Binomial Distribution
#To find probabilities for probability distributions for a binomial experiment, you use dbinom(r,n,p) –finds P(X=r) with n trials and the probability of a success is p. pbinom(x,n,p) –finds P(x<=r) with n trials and the probability of a success is p.

#As an example:If there are 20 trials, the probability of a success is 0.01, and you want to find the probability of 9 success, this would be P(x=9). This means you use the dbinom command.

dbinom(9,20,0.01)

#If you want to find the probability of at most 3 successes, then you want P(x is less than or equal to 3), which would be the pbinom command

pbinom(3,20, 0.01)

#If you want to get the probabilities for all values of the random variable, use dbinom(0:n,n,p) As an example, If you have 20 trials, and the probability of a success is 0.01, then you would do the following to get all of the values of the random variable from 0 to 20.

dbinom(0:20, 20, 0.01)

#Scientific Notation
#Note: R doesn’t write numbers that are really small, more than 4 decimal places, in standard notation. Instead numbers are given in scientific notation. As an example 0.00000000532 would be written as 5.32e-9 in R. This is common with many computers and calculators. You can either write the number with all the decimal places or write using correct scientific notation (5.32X10^-9) and not the way that R writes it.
###################################
u <- runif(20)

## The following relations always hold :
punif(u) == u
dunif(u) == 1

var(runif(10000))  #- ~ = 1/12 = .08333

punif(8+0.577, min=6, max=10)

p1 <- 1 - punif(8.577, 6, 10)
round(p1,3)
###################################
# Normal Distributions

#To find probabilities for the normal distribution that shows you the graph for P(x is less than r), you use xpnorm(r, mean= , sd= ). If you want the probability for P(x is more than r), then use xprnom(r, mean=, sd=, lower.tail=FALSE). To find the P(a is less than x which is less than b), then you do xpnorm(b, mean=, sd=)-xprnom(a, mean=, sd=). To find a data value when you know the area to the left you use xqnorm(area to the left, mean= , sd= ). This also show the graph. As an example, if the mean is 272 and the standard deviation is 9, and you want P(x<250), then you do
library(mosaic)
xpnorm(250, mean = 272, sd = 9)

#If know the mean is 272 and the standard deviation is 9, you know the probability to the left of an x value is 10%, and you want to find x, then you do

xqnorm(0.1, mean = 272, sd = 9)

####################################################
#Student’s t distribution
#To find probabilities for the t distribution use xpt(r, df = degrees of freedom) –finds P(t less than r) with degrees of freedom equal to df. Use xqt(p, df = degrees of freedom) to find when you know the P(t less than r_ with degrees of freedom equal to df and you want to know r. Example:If you know the degrees of freedom is 52, and you want to find P(t less than -7.707) then do
                                                                                                                                                                                                              
xpt(-7.707, df = 52)  

######################################################
#Chi-Squared Distribution
#To find probabilities for the chi-square distribution use xpchisq(, df = degrees of freedom, lower.tail=FALSE)
xpchisq(4, df=3, lower.tail=FALSE)



######################################################
xpnorm(650, 500, 100)
xqnorm(.75, 500, 100)
xpnorm(-3:3, return = "plot", system = "gg") |> 
  gf_labs(title = "My Plot", x = "") |> 
  gf_theme(theme_bw())

## Not run: 
if (rstudio_is_available() & require(manipulate)) {
  manipulate(xpnorm(score, 500, 100, verbose = verbose),
             score = slider(200, 800),
             verbose = checkbox(TRUE, label = "Verbose Output")
  )
}

##################################################
p = 0.75
n = 3

# P(X=3)
dgeom(x = n-1, prob = p)

# P(X<=3)
pgeom(q = n-1, prob = p)

# simulated
mean(rgeom(n = 10000, prob = p) == 3)

library(dplyr)
library(ggplot2)

data.frame(x = 0:10, prob = dgeom(x = 0:10, prob = p)) %>%
  mutate(Failures = ifelse(x == n, n, "other")) %>%
  ggplot(aes(x = factor(x), y = prob, fill = Failures)) +
  geom_col() +
  geom_text(
    aes(label = round(prob,2), y = prob + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of X = 3 Failures Prior to First Success",
       subtitle = "Geometric(.75)",
       x = "Failures prior to first success (x)",
       y = "Probability") 
################################################################
library(tigerstats)
pnormGC(72, region="below", mean=70,
        sd=3,graph=TRUE)

pnormGC(c(-1.96,1.96),region="between",mean=0,
        sd=1,graph=TRUE)

######################################################

library(mosaic)

pdist("norm", -2:2)

pdist("norm", seq(80,120, by = 10), mean = 100, sd = 10)

pdist("chisq", 2:4, df = 3)

pdist("f", 0.458, df1 = 2, df2 = 10)


library(mosaic)
pdist("gamma", 5, shape = 4, rate = 4.3)

#######################################################
##### Using R library functions for the F distribution

#### probability density function
n1 = 10
n2 = 12
r1 = n1-1
r2 = n2-2
F = 2.5

F_density = df(F, r1, r2)
F_density = round(F_density, digits=2)
print(paste("probability density for F = ", F, " and degree of freedom = ", r1," and ", r2, " is ", F_density))

### Generating the curve of F distribution probability density

x = seq(0,5,0.1)

r1 = 10
r2 = 12

string = "P(F,r1=10, r2=12)"

curve(df(x, r1, r2), xlim=c(0,4), xlab="F", ylab=string, lwd=1.5, cex.lab=1.2, col="blue", main="F distribution", font.lab=2)


#### Generating cumulative probability (p-value) above upto a F value in a F distribution

#######  pf(F, r1, r2) generates cumulative probability from 0 to given F value.
#######  The probability of having a value above F is 1 - pf(F, r1, r2)

r1 = 10
r2 = 12

F = 2.8
pvalue = pf(F, r1, r2)
pvalue = round(pvalue,3)
print(paste("cumulative probability from F = 0 to ", F, "is ", pvalue))


#### Generating F value for which the cumulative probability from 0 to F is p.
###  The function qf(p,r1, r2) returns F value at which cumulative probability is p.

p = 0.95  ## cumulative probabilitu from 0 to F.
F = qf(p, r1, r2)
F = round(F, digits=3)
print(paste("F value for a cumulative probability p = ", p, "is ", F))


#### Generating random deviates from a t distribution
### rf(m, f1, f2) returns a vector of m random deviates from a F of given F(r1,r2)

r1 = 10
r2 = 12
ndev = rf(4, r1, r2)
ndev = round(ndev,digits=3)
print("Four random deviates from F distribution with (10, 12) degrees of freedom : ")
print(ndev)

X11()

## plotting the histogram of 100000 random deviates from unit Gaussian:
r1 = 10
r2 = 12
hist(rf(100000, r1, r2), breaks=60, xlim = c(0,10), ylim = c(0, 40000), col="purple", main="histogram of F deviates")
#######################################################
dbinom(9,20,0.261)
success <- 0:20

plot(success,c,
     type='h',
     main='Binomial Distribution (n=20, p=0.261)',
     ylab='Probability',
     xlab ='# Successes',
     lwd=10, col = "red3")

summary(dbinom(0:20, 20, 0.261))
####################################################
# Install necessary packages
#install.packages("fitdistrplus")

# Load the packages
library(fitdistrplus)
library(ggplot2)

# Generating Random Weibull Data
# You can generate random values from a Weibull distribution in R using the rweibull() function.

# Generate random data from a Weibull distribution
set.seed(123)
weibull_data <- rweibull(n = 1000, shape = 2, scale = 0.1)
mean(weibull_data)
# Display the first few values
head(weibull_data)

# Visualizing the Weibull Distribution
# You can visualize the generated data using a histogram with a density curve.

# Plot histogram with density
ggplot(data.frame(weibull_data), aes(x = weibull_data)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "skyblue") +
  stat_function(fun = dweibull, args = list(shape = 1.5, scale = 2), color = "red", size = 1) +
  labs(title = "Weibull Distribution", x = "Values", y = "Density") +
  theme_minimal()

# Fit a Weibull distribution to the generated data
# To fit a Weibull distribution to data, you can use the fitdist() function from the fitdistrplus package.

# Visualizing the Fit
# The fitdistrplus package offers convenient functions for visualizing how well the Weibull distribution in R fits the data.

# Estimating Weibull Parameters from Data

fit <- fitdist(weibull_data, "weibull")

# Display the estimated parameters
print(fit)

# Visualizing the Fit
# The fitdistrplus package offers convenient functions for visualizing how well the Weibull distribution in R fits the data.
# Plot the fitted distribution
plot(fit)

# Calculating Weibull Probability Functions
# You can calculate the PDF, CDF, and quantile values using the following R functions:
  
#dweibull(x, shape, scale): PDF
#pweibull(q, shape, scale): CDF
#qweibull(p, shape, scale): Quantiles

wbl <- function(x, scale, shape){
  round(1-exp(-(scale*x)^shape),4)
}

wbl(5, 0.1, 2)
1-wbl(15, 0.1, 2)


# Calculate the PDF at x = 1.5
pdf_value <- dweibull((0:5), shape = 2, scale = 2)
print(pdf_value)

# Calculate the CDF at x = 1.5
cdf_value <- pweibull((0:5), shape = 1, scale = 2)
print(cdf_value)

# Find the quantile for probability 0.5
quantile_value <- qweibull(0.5, shape = 100, scale = 2)
print(quantile_value)

# Draw multiple Weibull distributions with different shapes
# We can also draw multiple Weibull distributions in R with different shapes, scales and range by specifying different colors to each distribution using curve function. The curve function Draws a curve corresponding to a function over the interval [from, to]. curve can plot also an expression in the variable xname, default x.

curve(dweibull(x, shape=2, scale=1), 
      from=0, to=5, col='blue', lwd=3, cex.lab=1.5, cex.axis = 1.25) 

curve(dweibull(x, shape=3, scale=2), 
      from=0, to=7, col='red', add=TRUE, lwd=3) 

curve(dweibull(x, shape=4, scale=3), 
      from=0, to=10, col='purple', add=TRUE, lwd=3) 

legend("top", c(expression(paste( , lambda)),"scale=1", "scale=2", "scale=3"),
       lty = c(0, 1, 1, 1), col = c("blue", "red", "purple"), box.lty = 0, lwd = 2, cex=2)



############################################
library(extraDistr)
x <- rrayleigh(1e5, 13)
hist(x, 100, freq = FALSE)
curve(drayleigh(x, 13), 0, 60, col = "red", add = TRUE)
hist(prayleigh(x, 13)) 
plot(ecdf(x))
curve(prayleigh(x, 13), 0, 60, col = "red", lwd = 2, add = TRUE) 

###########################################################\
Examples
# Density of a triangular distribution with parameters 
# min=10, max=15, and mode=12, evaluated at 12, 13 and 14: 

library(EnvStats)

dtri(12:14, 10, 15, 12) 
#[1] 0.4000000 0.2666667 0.1333333

#----------

# The cdf of a triangular distribution with parameters 
# min=2, max=7, and mode=5, evaluated at 3, 4, and 5: 

ptri(3:5, 2, 7, 5) 
#[1] 0.06666667 0.26666667 0.60000000

#----------

# The 25'th percentile of a triangular distribution with parameters 
# min=1, max=4, and mode=3: 

qtri(0.25, 1, 4, 3) 
#[1] 2.224745

#----------

# A random sample of 4 numbers from a triangular distribution with 
# parameters min=3 , max=20, and mode=12. 
# (Note: the call to set.seed simply allows you to reproduce this example.)

set.seed(10) 
rtri(4, 3, 20, 12) 
#[1] 11.811593  9.850955 11.081885 13.539496

##########################################################

x <- seq(0, 1, length = 21)
dbeta(x, 1, 1)
pbeta(x, 1, 1)

## Visualization, including limit cases:
pl.beta <- function(a,b, asp = if(isLim) 1, ylim = if(isLim) c(0,1.1)) {
  if(isLim <- a == 0 || b == 0 || a == Inf || b == Inf) {
    eps <- 1e-10
    x <- c(0, eps, (1:7)/16, 1/2+c(-eps,0,eps), (9:15)/16, 1-eps, 1)
  } else {
    x <- seq(0, 1, length = 1025)
  }
  fx <- cbind(dbeta(x, a,b), pbeta(x, a,b), qbeta(x, a,b))
  f <- fx; f[fx == Inf] <- 1e100
  matplot(x, f, ylab="", type="l", ylim=ylim, asp=asp, lwd=3,
          main = sprintf("[dpq]beta(x, a=%g, b=%g)", a,b))
  abline(0,1,     col="gray", lty=3, lwd=3)
  abline(h = 0:1, col="gray", lty=3, lwd=3)
  legend("top", paste0(c("d","p","q"), "beta(x, a,b)"),
         col=1:3, lty=1:3, bty = "n")
  invisible(cbind(x, fx))
}
pl.beta(3,1)

pl.beta(2, 4)
pl.beta(3, 7)
pl.beta(3, 7, asp=1)

pl.beta(0, 0)   ## point masses at  {0, 1}

pl.beta(0, 2)   ## point mass at 0 ; the same as
pl.beta(1, Inf)

pl.beta(Inf, 2) ## point mass at 1 ; the same as
pl.beta(3, 0)

pl.beta(Inf, Inf)# point mass at 1/2
##############################################
# Exp

# Generate 1000 random values from an
# exponential distribution with rate = 0.5

set.seed(123) # for reproducibility
values <- rexp(n = 1000, rate = 0.5)

# Plot a histogram of the generated values
hist(
  values,
  freq = FALSE,
  main = "Exponential Distribution",
  xlab = "Values",
  ylab = "Density"
)

# Add the theoretical density curve to the plot
curve(dexp(x, rate = 0.5),
      add = TRUE,
      col = "red",
      lwd = 2)

# calculate the quantile of Exponential distribution 
# with lambda = 0.5at probability level 0.75
qexp(1, rate = 4.3*0.25)

# calculate the PDF of Exponential distribution 
# with lambda = 0.5 at x = 1
lambda = 0.05 
length =.10
x=28
dexp(x, rate = lambda)
dexp(length, rate = lambda)

# calculate the CDF of Exponential distribution 
# with lambda = 0.5 at x = 1
pexp(28, rate = 0.05, lower.tail = F)
pexp(7, rate = 0.05)
exp(-0.05*28)
1-exp(-0.05*7)

pexp(0.1, rate = 4.3)
pexp(0.3, rate = 4.3, lower.tail = F)

#Exponential density function
# Grid of X-axis values
x <- seq(0, 2, 0.1)

# lambda = 2
plot(x, dexp(x, 20), type = "l",
     ylab = "", lwd = 3, col = "red")
# lambda = 1
lines(x, dexp(x, rate = 7), col = "blue", lty = 1, lwd = 3)

# Adding a legend
legend("center", c(expression(paste(, lambda)), "20", "7"),
       lty = c(0, 1, 1), col = c("blue", "red"), box.lty = 0, lwd = 2)

#In order to plot the area under an exponential curve with a single line of code you can use the following function that we have developed:
  
  # rate: lambda parameter
  # lb: lower bound of the area
  # ub: upper bound of the area
  # acolor: color of the area
  # ...: additional arguments to be passed to the lines function
  exp_area <- function(rate = 1, lb, ub, acolor = "lightgray", ...) {
    x <- seq(0, 12/rate, 0.01) 
    
    if (missing(lb)) {
      lb <- min(x)
    }
    if (missing(ub)) {
      ub <- max(x)
    }
    
    x2 <- seq(lb, ub, length = 100)    
    plot(x, dexp(x, rate = rate), type = "n", ylab = "")
    
    y <- dexp(x2, rate = rate)
    polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
    lines(x, dexp(x, rate = rate), type = "l", ...)
  }
#As an example, you could plot the area under an exponential curve of rate 0.5 between 0.5 and 5 with the following code:
  
  exp_area(rate = 0.5, lb = 0.5, ub = 5, acolor = "dodgerblue", col = "red", lwd =2)
  
  exp_area(rate = 0.2, ub = 3, acolor = rgb(0, 0, 1, alpha = 0.5), col = "red", lwd =2)
  text(1, 0.075, "45.12%", srt = 90, col = "white", cex = 1.25)

  pexp(10, rate = 0.2, lower.tail = FALSE) # 0.1353353 or 13.53%
  
  
  exp_area(rate = 0.2, lb = 10, acolor = rgb(0, 0, 1, alpha = 0.5), col = "red", lwd=2)
  arrows(8, 0.1, 11, 0.015, length = 0.1, lwd = 2)
  text(8, 0.12, "13.53%", cex = 1.2)
  ##############################################


dchisq(1, df = 1:3)
pchisq(1, df =  3)
pchisq(1, df =  3, ncp = 0:4)  # includes the above

x <- 1:10
## Chi-squared(df = 2) is a special exponential distribution
all.equal(dchisq(x, df = 2), dexp(x, 1/2))
all.equal(pchisq(x, df = 2), pexp(x, 1/2))

## non-central RNG -- df = 0 with ncp > 0:  Z0 has point mass at 0!
Z0 <- rchisq(100, df = 0, ncp = 2.)
graphics::stem(Z0)

# }
# NOT RUN {
## visual testing
## do P-P plots for 1000 points at various degrees of freedom
L <- 1.2; n <- 1000; pp <- ppoints(n)
op <- par(mfrow = c(3,3), mar = c(3,3,1,1)+.1, mgp = c(1.5,.6,0),
          oma = c(0,0,3,0))
for(df in 2^(4*rnorm(9))) {
  plot(pp, sort(pchisq(rr <- rchisq(n, df = df, ncp = L), df = df, ncp = L)),
       ylab = "pchisq(rchisq(.),.)", pch = ".")
  mtext(paste("df = ", formatC(df, digits = 4)), line =  -2, adj = 0.05)
  abline(0, 1, col = 2)
}
mtext(expression("P-P plots : Noncentral  "*
                   chi^2 *"(n=1000, df=X, ncp= 1.2)"),
      cex = 1.5, font = 2, outer = TRUE)
par(op)
# }
# NOT RUN {
## "analytical" test
lam <- seq(0, 100, by = .25)
p00 <- pchisq(0,      df = 0, ncp = lam)
p.0 <- pchisq(1e-300, df = 0, ncp = lam)
stopifnot(all.equal(p00, exp(-lam/2)),
          all.equal(p.0, exp(-lam/2)))




dexp(1) - exp(-1) #-> 0

## a fast way to generate *sorted*  U[0,1]  random numbers:
rsunif <- function(n) { n1 <- n+1
cE <- cumsum(rexp(n1)); cE[seq_len(n)]/cE[n1] }
plot(rsunif(1000), ylim=0:1, pch=".")
abline(0,1/(1000+1), col=adjustcolor(1, 0.5))




x <- seq(0.001, 5, len = 100)
nu <- 4
stopifnot(all.equal(2*pt(x,nu) - 1, pf(x^2, 1,nu)),
          ## upper tails:
          all.equal(2*pt(x,     nu, lower=FALSE),
                    pf(x^2, 1,nu, lower=FALSE)))

## the density of the square of a t_m is 2*dt(x, m)/(2*x)
# check this is the same as the density of F_{1,m}
all.equal(df(x^2, 1, 5), dt(x, 5)/x)

## Identity:  qf(2*p - 1, 1, df) == qt(p, df)^2  for  p >= 1/2
p <- seq(1/2, .99, length = 50); df <- 10
rel.err <- function(x, y) ifelse(x == y, 0, abs(x-y)/mean(abs(c(x,y))))
# }
# NOT RUN {
quantile(rel.err(qf(2*p - 1, df1 = 1, df2 = df), qt(p, df)^2), .90)  # ~= 7e-9


#####################################
# Binomial Distribution

require(graphics)
x <- 0:11
dnbinom(x, size = 1, prob = 1/2) * 2^(1 + x) # == 1
126 /  dnbinom(0:8, size  = 2, prob  = 1/2) #- theoretically integer

# }
# NOT RUN {
## Cumulative ('p') = Sum of discrete prob.s ('d');  Relative error :
summary(1 - cumsum(dnbinom(x, size = 2, prob = 1/2)) /
          pnbinom(x, size  = 2, prob = 1/2))
# }
# NOT RUN {
x <- 0:15
p=0.6
size <- (1:20)/4
persp(x, size, dnb <- outer(x, size, function(x,s) dnbinom(x, s, prob = p)),
      col = "lightblue", ltheta = 120, shade = 0.75, #ticktype = "detailed",
      xlab = "x", ylab = "s", zlab = "density", theta = 150)
title(tit <- "negative binomial density(x,s, pr = 0.6)  vs.  x & s")

image  (x, size, log10(dnb), main = paste("log [", tit, "]"))
contour(x, size, log10(dnb), add = TRUE)

## Alternative parametrization
x1 <- rnbinom(500, mu = 4, size = 1)
x2 <- rnbinom(500, mu = 4, size = 10)
x3 <- rnbinom(500, mu = 4, size = 100)
h1 <- hist(x1, breaks = 20, plot = FALSE)
h2 <- hist(x2, breaks = h1$breaks, plot = FALSE)
h3 <- hist(x3, breaks = h1$breaks, plot = FALSE)
barplot(rbind(h1$counts, h2$counts, h3$counts),
        beside = TRUE, col = c("red","blue","cyan"),
        names.arg = round(h1$breaks[-length(h1$breaks)]))

r = 3
p = 0.60
n = 7 - r
# exact
dnbinom(x = 6, size = r, prob = p)

x1 <- dnbinom(x=0, size = r, prob = p)
x2 <- dnbinom(x=1, size = r, prob = p)
x3 <- dnbinom(x=2, size = r, prob = p)
x4 <- dnbinom(x=3, size = r, prob = p)
x1+x2+x3+x4

r=3
p=0.6
x6 <- choose(6-1,r-1)*(1-p)^(6-r)*p^(r)
x5 <- choose(5-1,r-1)*(1-p)^(5-r)*p^(r)
x4 <- choose(4-1,r-1)*(1-p)^(4-r)*p^(r)
x3 <- choose(3-1,r-1)*(1-p)^(3-r)*p^(r)
x3+x4+x5+x6

###################################
#Poisson
# The probability of having sixteen or less cars crossing the bridge in a particular minute is given by the function ppois.

x0 <- ppois(0, lambda=3)   # lower tail 
x1 <- ppois(1, lambda=3)
x2 <- ppois(2, lambda=3)

x2 <- 1-ppois(2, lambda=3) # lower tail
round(x2,3)

%P(X>=3)
x2 <- ppois(2, lambda=3, lower=FALSE)  # upper tail 
round(x2,3)

####################################


require(graphics)

dnorm(0) == 1/sqrt(2*pi)
dnorm(1) == exp(-1/2)/sqrt(2*pi)
dnorm(1) == 1/sqrt(2*pi*exp(1))

## Using "log = TRUE" for an extended range :
par(mfrow = c(1,1))
plot(function(x) dnorm(x, log = TRUE), -60, 50,
     main = "log { Normal density }")
curve(log(dnorm(x)), add = TRUE, col = "red", lwd = 2)
mtext("dnorm(x, log=TRUE)", adj = 0)
mtext("log(dnorm(x))", col = "red", adj = 1)

plot(function(x) pnorm(x, log.p = TRUE), -50, 10,
     main = "log { Normal Cumulative }")
curve(log(pnorm(x)), add = TRUE, col = "red", lwd = 2)
mtext("pnorm(x, log=TRUE)", adj = 0)
mtext("log(pnorm(x))", col = "red", adj = 1)

## if you want the so-called 'error function'
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
## (see Abramowitz and Stegun 29.2.29)
## and the so-called 'complementary error function'
erfc <- function(x) 2 * pnorm(x * sqrt(2), lower = FALSE)
## and the inverses
erfinv <- function (x) qnorm((1 + x)/2)/sqrt(2)
erfcinv <- function (x) qnorm(x/2, lower = FALSE)/sqrt(2)





require(graphics)

1 - pt(1:5, df = 1)
qt(.975, df = c(1:10,20,50,100,1000))

tt <- seq(0, 10, len = 21)
ncp <- seq(0, 6, len = 31)
ptn <- outer(tt, ncp, function(t, d) pt(t, df = 3, ncp = d))
t.tit <- "Non-central t - Probabilities"
image(tt, ncp, ptn, zlim = c(0,1), main = t.tit)
persp(tt, ncp, ptn, zlim = 0:1, r = 2, phi = 20, theta = 200, main = t.tit,
      xlab = "t", ylab = "non-centrality parameter",
      zlab = "Pr(T <= t)")

plot(function(x) dt(x, df = 3, ncp = 2), -3, 11, ylim = c(0, 0.32),
     main = "Non-central t - Density", yaxs = "i")





require(graphics)

-log(dpois(0:7, lambda = 1) * gamma(1+ 0:7)) # == 1
Ni <- rpois(50, lambda = 4); table(factor(Ni, 0:max(Ni)))

1 - ppois(10*(15:25), lambda = 100)  # becomes 0 (cancellation)
ppois(10*(15:25), lambda = 100, lower.tail = FALSE)  # no cancellation

par(mfrow = c(1, 1))
x <- seq(-0.01, 5, 0.01)
plot(x, ppois(x, 1), type = "s", ylab = "F(x)", main = "Poisson(1) CDF")
plot(x, pbinom(x, 100, 0.01), type = "s", ylab = "F(x)",
     main = "Binomial(100, 0.01) CDF")

## The (limit) case  lambda = 0 :
stopifnot(identical(dpois(0,0), 1),
          identical(ppois(0,0), 1),
          identical(qpois(1,0), 0))








###############################################################
# Display the Chi-squared distributions with
#  1, 2, 4, 8, 16, and 32 degrees of freedom. 

par(mfrow = c(1,1))
x <- seq(0, 40, length=200)
hx <- rep(0,200)

degf <- c(1,2,4,8,16,32)
colors <-  c("red",  "orange", "green", "blue", "black", "violet")
labels <- c("df=1",  "df=2",  "df=4",  "df=8",  "df=16",  "df=32")

plot(x, hx, type="n", lty=2, lwd=2, xlab="x value",
     ylab="Density", ylim=c(0,0.7), xlim=c(0,40), las=1,
     xaxp=c(0,40,10),
     main="Chi-Squared Distribution \n 1, 2, 4, 8, 16, 32 Degrees of Freedom"
)

for (i in 1:6){
  lines(x, dchisq(x,degf[i]), lwd=2, col=colors[i], lty=1)
}
abline(h=0)
abline(h=seq(0.1,0.7,0.1), lty=3, col="darkgray")
abline(v=0)
abline(v=seq(2,40,2), lty=3, col="darkgray")
legend("topright", inset=.05, title="Degrees of Freedom",
       labels, lwd=2, lty=1, col=colors)


for (j in 1:6 ){
  plot(x, hx, type="n", lty=2, lwd=2, xlab="x value",
       ylab="Density", ylim=c(0,0.7), xlim=c(0,40), las=1,
       xaxp=c(0,40,10),
       main=paste("Chi-Squared Distribution:",degf[j]," Degrees of Freedom")
  )
  
  for (i in j:j){
    lines(x, dchisq(x,degf[i]), lwd=2, col=colors[i], lty=1)
  }
  abline(h=0)
  abline(h=seq(0.1,0.7,0.1), lty=3, col="darkgray")
  abline(v=0)
  abline(v=seq(2,40,2), lty=3, col="darkgray")
  legend("topright", inset=.05, title="Degrees of Freedom",
         labels[j], lwd=2, lty=1, col=colors[j])
  
}

# look at areas to the right of 4 for 6 different 
# options on the degrees of freedom
pchisq(4,1, lower.tail=FALSE)
pchisq(4,2, lower.tail=FALSE)
pchisq(4,4, lower.tail=FALSE)
pchisq(4,8, lower.tail=FALSE)
pchisq(4,16, lower.tail=FALSE)
pchisq(4,32, lower.tail=FALSE)

# look at a left tail
pchisq(1.239,7)

qchisq(0.01,7)
pchisq(1.239042,7)

qchisq(0.01,7,lower.tail=FALSE)
pchisq(18.47531,7,lower.tail=FALSE)

pchisq(2.34,6)
pchisq(15.34, 9, lower.tail=FALSE)
pchisq(6.66, 17) + pchisq(27.34, 17, lower.tail=FALSE)
pchisq(25.41, 14) - pchisq(5.25, 14)
qchisq(0.0333, 5)
qchisq(0.125, 25, lower.tail=FALSE)
qchisq( 0.125, 11)
qchisq( 0.125, 11, lower.tail=FALSE )
qchisq( 0.01665, 23)
qchisq( 0.01665, 23, lower.tail=FALSE )

#############################################
dtri(12:14, 10, 15, 12) 
ptri(190, 180, 240, 210) 
qtri(0.25, 1, 4, 3) 
set.seed(10) 
rtri(4, 3, 20, 12)

ptri(190, 180, 240, 210) 

ptri(230, 180, 240, 210) - ptri(200, 180, 240, 210)

ptri(210, 180, 240, 210) 
##############################################
library(VGAM)
rate <- exp(2); myshape <- 3
edata <- data.frame(y = rep(0, nn <- 1000))
for (ii in 1:myshape)
  edata <- transform(edata, y = y + rexp(nn, rate = rate))
fit <- vglm(y ~ 1, erlang(shape = myshape), edata, trace = TRUE)
coef(fit, matrix = TRUE)
Coef(fit)  # Answer = 1/rate
1/rate
summary(fit)


library(distributions3)
set.seed(27)

X <- Erlang(5, 2)
X

Y <- as.data.frame(random(X, 30))
Y

plot(random(X, 30))

mean(Y)

pdf(X, 2)
log_pdf(X, 2)

cdf(X, 4)
quantile(X, 0.7)

cdf(X, quantile(X, 0.7))
quantile(X, cdf(X, 7))

sample_data <- data.frame(x=1:30, 
                          y=c(random(Erlang(5,2),30))) 

# fit polynomial regression models up to degree 5 
linear_model1 <- lm(y~x, data=sample_data) 
linear_model2 <- lm(y~poly(x,2,raw=TRUE), data=sample_data) 
linear_model3 <- lm(y~poly(x,3,raw=TRUE), data=sample_data) 
linear_model4 <- lm(y~poly(x,4,raw=TRUE), data=sample_data) 
linear_model5 <- lm(y~poly(x,5,raw=TRUE), data=sample_data) 

# create a basic scatterplot  
plot(sample_data$x, sample_data$y)

# define x-axis values 
x_axis <- seq(1, 40, length=40) 

# add curve of each model to plot 
lines(x_axis, predict(linear_model1, data.frame(x=x_axis)), col='green') 
lines(x_axis, predict(linear_model2, data.frame(x=x_axis)), col='red') 
lines(x_axis, predict(linear_model3, data.frame(x=x_axis)), col='purple') 
lines(x_axis, predict(linear_model4, data.frame(x=x_axis)), col='blue') 
lines(x_axis, predict(linear_model5, data.frame(x=x_axis)), col='orange')

X1 <- rgamma(40, shape=2, rate = 5)
X1 <- as.data.frame(X1)
fit <- vglm(y ~ 1, erlang(shape = 2), X, trace = TRUE)
fit


rate <- exp(2); myshape <- 3
edata <- data.frame(y = rep(0, nn <- 100))
for (ii in 1:myshape)
  edata <- transform(edata, y = y + rexp(nn, rate = rate))
fit <- vglm(y ~ 1, erlang(shape = myshape), edata, trace = TRUE)
summary(fit)

x_axis <- cbind(seq(1, 100, length=100))
plot(x_axis, edata)

X1<-erlang(shape = 2)
X1

set.seed(27)
X <- Erlang(5, 2)
dat <- random(X, 100000)
hist(random(X, 100000), breaks=60, xlim = c(0,10), ylim = c(0, 8000), col="purple", main="histogram of Erlang (5, 2) deviates")

#df <- data.frame(x = (1:10000), dist_name = "Erlang", y = dgamma(x, shape = 2, rate = 5))

x <- seq(from = 0, to = 10, length.out = 10000) # Define the density domains
ylim <- c(0, 5)

df<-data.frame(x = x, dist_name = "Gamma", y = dgamma(x, shape = 5, rate = 2))

ggplot(data = df, aes(x = x, y = y)) +
  geom_line(size = 2, colour = "dodgerblue") +
  ggtitle("Erlang(5, 2)") + 
  theme(plot.title = element_text(color="red", size=20))

ggplot(data = df, aes(x = x, y = y)) +
  geom_histogram(aes(x=x,y = y), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)
ggplot(data=dat, aes(x=x, y=y)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") 

library(fitdistrplus)
Gama <- rgamma(10000, 5, 2) #+ rnorm(30, 0, .03)
Erlang <- random(Erlang(5, 2),10000) 
fit <- fitdist(Erlang, distr = "gamma", method = "mle")
#par(mar=c(1, 1, 1, 1))
plot(fit, lwd=2, col="dodgerblue")

library(Distributacalcul)
pErlang(2.3356, shape=5, rate = 2, scale = 1/rate, lower.tail = F)

X <- Erlang(5, 2)
quantile(X,0.5)

X1 <- Erlang(2, 5)
quantile(X,0.5)
pErlang(0.3356, shape=2, rate = 5, scale = 1/rate, lower.tail = F)


if(!require('Distributacalcul')) {
  install.packages('Distributacalcul')
  library('Distributacalcul')
}
SL_beta(d = 0.3, shape1 = 4, shape2 = 5)




######################################

library(actuar)
library(VGAM)

## Example 4:GB2
gb2density <- function(x,shape1,shape2,shape3,scale){
  mu <- log(scale)
  sigma <- 1/shape3
  xt <- (log(x)-mu)/sigma
  logexpxt<-ifelse(xt>23,yt,log(1+exp(xt)))
  logdens <- shape1*xt - log(sigma) - log(beta(shape1,shape2)) - (shape1+shape2)*logexpxt -log(x) 
  exp(logdens)
}
x<- seq(0,400,by=1)

alpha1<-5
alpha2<-4 
gamma <-2
theta <- seq(150,250,50)

# varying the scale parameter
plot(x, gb2density(x, shape1=alpha1,shape2=alpha2,shape3=gamma, scale = theta[1]), 
     type = "l", ylab = "Gen Beta 2 density",
     main = 
       expression(paste("GB2 density with ", alpha[1], "=5,", alpha[2], "=4,", alpha[3], 
                        "=2, and varying scale (",theta, ") parameters")) )

for(k in 2:length(theta)){
  lines(x,gb2density(x,shape1=alpha1,shape2=alpha2,shape3=gamma, scale = theta[k]), col = k)
}
legend("topleft", c(expression(theta~'=150'), expression(theta~'=200'), expression(theta~'=250')), lty=1, cex=0.6,col = 1:3)


expValBeta(shape1 = 3, shape2 = 5)

stopLossBeta(d = .3, shape1 = 4, shape2 = 7)
meanExcessBeta(d = .3, shape1 = 4, shape2 = 7)
             