runElectionSimulations <- function(y1, y2, y3, num_draws){
  # Draw 1000 points from Dirichlet distribution
  df <- as.data.frame(gtools::rdirichlet(num_draws, c(y1 + 1, y2+1, y3+1)))
  
  # Rename columns
  names(df) <- c("theta1", "theta2", "theta3")
  
  # Calculate difference in support in each simulation
  df <- df %>% dplyr::mutate(support_difference = theta1 - theta2)
  
  return(df)
}

plotElectionSupportDifference <- function(df, median){
  
  x <- c(0, median)
  labels <- c("Tie in support", "Median")
  line_df <- data.frame("x"=x, "label"=labels)
  
  ggplot2::ggplot(df, aes(support_difference)) +
    geom_histogram() +
    geom_vline(data = line_df, aes(xintercept=x, color=label, linetype=label)) +
    theme_bw() + 
    theme(legend.position="bottom") +
    labs(color = "Values of Interest", linetype = "Values of Interest") + 
    xlab("Difference in Support") + 
    ylab("Count")
    
}
