runSimulations <- function(y1, y2, y3, num_draws){
  # Draw 1000 points from Dirichlet distribution
  df <- as.data.frame(gtools::rdirichlet(num_draws, c(y1 + 1, y2+1, y3+1)))
  
  # Rename columns
  names(df) <- c("theta1", "theta2", "theta3")
  
  # Calculate difference in support in each simulation
  df <- df %>% dplyr::mutate(support_difference = theta1 - theta2)
  
  return(df)
}
