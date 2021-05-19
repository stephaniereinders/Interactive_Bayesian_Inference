generateSingleData <- function(sample_size_n, probability_theta){
  # Draw sample_size_n samples from a Bernouilli distribution and return in a dataframe.
  # NOTE: Bernouilli distribution = Binomial distribution with n=1: p(x) = p^x(1-p)^(1-x)
  obs <- stats::rbinom(n = sample_size_n, size = 1, prob=probability_theta)
  obs <- as.factor(unlist(lapply(obs, function(x) if (x==1){"yes"} else {"no"})))
  df <- data.frame("observations" = obs)
  return(df)
}
  

plotSingleData <- function(df, sample_size_n, num_yes_y){
  # Make a dotplot of the number of yes and no responses
  
  # Make max height proportional to num yes or num no, whichever is larger. (Each circle will be 1/n in diameter 
  # so total height of successes or failures circles will be y*(1/n) or (n-y)*(1/n)).) 
  num_no <- sample_size_n - num_yes_y
  if (num_yes_y >= num_no){
    max_height <- num_yes_y/sample_size_n
  } else {
    max_height <- num_no/sample_size_n
  }
  
  # Make plot
  p <- ggplot(df, aes(observations)) + 
    geom_dotplot(binwidth = 1/sample_size_n) +  # make each dot 1/sample_size_n in diameter
    theme_bw() + 
    coord_fixed(ylim = c(0, max_height)) +  # fix y-axis
    scale_y_continuous(NULL, breaks=NULL)  # hide y-axis labels
  
  return(p)
}

plotBinomialDist <- function(sample_size_n, sample_proportion){
  # Plot the binomial distribution with sample size n and probability equal to the sample proportion
  
  # Generate a sequence of all possible numbers of yeses (I.e. 1, 2,...,n). 
  # Let the sequence go up to n+2 to just to make the plot look nicer.
  yeses <- seq(0, sample_size_n+2, by = 1)  
  
  # Calculate the probability of obtaining each number of yeses in n observations
  probability <- dbinom(yeses, size=sample_size_n, prob=sample_proportion)
  
  # Create data frame
  df <- data.frame("yeses" = yeses, "probability" = probability)
  
  # Graph
  p <- ggplot(df, aes(x=yeses, y=probability)) + 
    geom_point() +
    theme_bw() +
    xlab("number of yes responses")
  
  return(p)
}

plotBetaDist <- function(sample_size_n, num_yes_y){
  # Plot the unnormalized beta distribution
  p <- ggplot() +
    geom_function(fun = function(x) (x^num_yes_y)*(1-x)^(sample_size_n - num_yes_y)) +
    theme_bw()
  
  return(p)
}

plotSingleEstimates <- function(prior_mean, posterior_mean, sample_proportion){
  # Make dataframe
  x <- c(prior_mean, posterior_mean, sample_proportion)
  x_labels <- c("prior mean", "posterior mean", "sample proportion")
  y <- c(0, 0, 0)
  df <- data.frame("x" = x, "x_labels" = x_labels, "y" = y)
  df$x_labels <- factor(df$x_labels, levels = c("prior mean", "posterior mean", "sample proportion"))  # fix order in legend
  
  # Plot
  p <- ggplot(df, aes(x=x, y=y)) + 
    geom_hline(yintercept=0) + 
    geom_point(size=5, aes(color=x_labels, shape=x_labels)) + 
    theme_bw() + 
    scale_color_manual(values=RColorBrewer::brewer.pal(n=3, name="Dark2")) + 
    coord_fixed(ylim = c(-0.025, 0.025)) +  # fix y-axis
    scale_y_continuous(NULL, breaks=NULL) +  # hide y-axis labels
    scale_x_continuous(NULL) +
    labs(color = "Estimate Type", shape = "Estimate Type") + 
    theme(legend.position = "bottom")
  
  return(p)
}