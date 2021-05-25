# library(ggplot2)
# 
# # Generate simulated data
# 
# # k = number of groups (dose levels)
# # xi = dose for group i
# # ni = number of animals in group i
# # yi = number of deaths in group i
# # thetai = probability of death in group i
# # alpha = intercept parameter of logistic regression model
# # beta = slope parameter of logistic regression model
# 
# # Assume logistic regression model: 
# #   logit(thetai) = alpha + beta*xi where logit(thetai) = log(thetai/(1-thetai))
# #   log(thetai/(1-thetai)) = alpha + beta*xi
# #   thetai = exp(alpha + beta*xi)/(1 + exp(alpha + beta*xi))
# 
# # Choose parameter values
# k <- 6
# alpha <- 0
# beta <- 3.67
# dose_min <- -0.9
# dose_max <- 0.9
# 
# #--- END USER INPUT
# # Generate data
# n <- rep(10, k)
# x <- seq(from=dose_min, to=dose_max, length.out=k)
# 
# # Calculate probability of death yi for each group i
# theta <- unlist(lapply(x, function(xi) exp(alpha + beta*xi)/(1 + exp(alpha + beta*xi))))
# y <- round(n*theta)
# 
# # Make dataframe
# df <- data.frame("x" = x, "n" = n, "y" = y, "theta" = theta)
# 
# # Sampling distribution: yi|thetai ~ Bin(ni, thetai)
# df %>% ggplot(aes(x, y)) + 
#   geom_point()
# 
# probability <- dbinom(seq(from=0, to=k, by=1), size=n[1], prob=theta[1])
# 
# 
# # Graph
# ggplot(df, aes(x=yeses, y=probability)) + 
#   geom_point() +
#   theme_bw() +
#   xlab("number of yes responses")
# 
# x <- seq(from=0, to=n[1], by=1)
# 
# 
# 
# probs1 <- dbinom(seq(from=0, to=n[1], by=1), size=n[1], prob=theta[1])
# 
# 
# 
# 
# dsim <- data.frame(x = x , y = y)
# 
# ggplot(data = dsim, aes(x=x, y=y)) + geom_point()
# 
# 
# 
