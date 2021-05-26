library(ggplot2)

# Generate simulated data

# k = number of groups (dose levels)
# xi = dose for group i
# ni = number of animals in group i
# yi = number of deaths in group i
# thetai = probability of death in group i
# alpha = intercept parameter of logistic regression model
# beta = slope parameter of logistic regression model

# Assumption 1: The outcome of the five animals within group i is exchangeable. 
# Model the outcomes of the five animals within group i as independent with the equal probabilities.
# Assumption 2: Give the parameters theta1, theta2, theta3, theta4, treat the outcomes in the four groups
# as independt from each other

# Model the dose-response relation with logistic regression model:
#   logit(thetai) = alpha + beta*xi where logit(thetai) = log(thetai/(1-thetai))
#   log(thetai/(1-thetai)) = alpha + beta*xi
#   thetai = exp(alpha + beta*xi)/(1 + exp(alpha + beta*xi))

# Sampling distribution: 
#     yi|thetai ~ Bin(ni, thetai)
#     yi|alpha, beta propto [logit^-1(alpha + beta*xi)]^yi[1 - logit^-1(alpha + beta*xi)]^(ni-yi)

# Joint distribution of alpha and beta
#     p(alpha, beta | y)

# Choose parameter values
k <- 6
alpha <- 0
beta <- 3.67
dose_min <- -0.9
dose_max <- 0.9

#--- END USER INPUT
# Generate data
n <- rep(10, k)
x <- seq(from=dose_min, to=dose_max, length.out=k)  # returns k evenly spaced points from points dose_min to dose_max  

# Calculate probability of death yi for each group i
theta <- unlist(lapply(x, function(xi) exp(alpha + beta*xi)/(1 + exp(alpha + beta*xi))))
y <- round(n*theta)

# Make dataframe
df <- data.frame("x" = x, "n" = n, "y" = y, "theta" = theta)





