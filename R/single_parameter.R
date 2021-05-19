generateSingleParameterData <- function(sample_size_n, probability_theta){
  # Draw sample_size_n samples from a Bernouilli distribution and return in a dataframe.
  # NOTE: Bernouilli distribution = Binomial distribution with n=1: p(x) = p^x(1-p)^(1-x)
  obs <- stats::rbinom(n = sample_size_n, size = 1, prob=probability_theta)
  obs <- as.factor(unlist(lapply(obs, function(x) if (x==1){"yes"} else {"no"})))
  df <- data.frame("observations" = obs)
  return(df)
}
  

makeDataDotplot <- function(df, sample_size_n, num_yes_y){
  # Make a dotplot of the 
  
  
  # Make max height proportional to num yes or num no, whichever is larger. (Each circle will be 1/n in diameter 
  # so total height of successes or failures circles will be y*(1/n) or (n-y)*(1/n)).) 
  num_no <- sample_size_n - num_yes_y
  if (num_yes_y >= num_no){
    max_height <- num_yes_y/sample_size_n
  } else {
    max_height <- num_no/sample_size_n
  }
  
  p <- ggplot(df, aes(observations)) + 
    geom_dotplot(binwidth = 1/sample_size_n) +  # make each dot 1/sample_size_n in diameter
    theme_bw() + 
    coord_fixed(ylim = c(0, max_height)) +  # fix y-axis
    scale_y_continuous(NULL, breaks=NULL)  # hide y-axis labels
  
  return(p)
}
