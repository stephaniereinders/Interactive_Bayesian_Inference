generateSingleParameterData <- function(sample_size_n, probability_theta){
  # Draw single_n samples from a Bernouilli distribution.
  # Bernouilli distribution = Binomial distribution with n=1: p(x) = p^x(1-p)^(1-x)
  obs <- stats::rbinom(n = sample_size_n, size = 1, prob=probability_theta)
  obs <- as.factor(unlist(lapply(obs, function(x) if (x==1){"yes"} else {"no"})))
  df <- data.frame("observations" = obs)
  return(df)
}
  

