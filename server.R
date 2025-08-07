library(shiny)
library(ggplot2)
library(dplyr)
library(gtools)  # used for Dirichlet distribution
source("R/bicycle_ownership.R")
source("R/election_polling.R")

shinyServer(function(input, output) {
  
  ###--- BICYCLE OWNERSHIP EXAMPLE ---########################################
  #--- Global Variables
  # Initialize and keep track of current variable values
  bike_vars <- reactiveValues(theta = 0.75,
                              n = 10,  # number of trials 
                              y = 5,  # number of successes
                              y_plus_1 = 6,  # parameter for beta distribution
                              n_plus_2 = 12,  # parameter for beta distribution
                              sample_proportion = 5/10,  # y/n
                              prior_mean = 1/2, 
                              posterior_mean = 6/12  # (y+1)/(n+2)
  )
  
  # Update global variables with current user input
  observe({
    # Update directly from user input
    bike_vars$theta <- input$bike_theta
    bike_vars$n <- input$bike_n
    bike_vars$n_plus_2 <- bike_vars$n + 2  # parameter for beta distribution
    
    # Update df from user input first to then update variable
    bike_vars$y <- dplyr::count(bike_df(), observations)["n"][2,]
    bike_vars$y_plus_1 <- bike_vars$y + 1  # parameter for beta distribution
    bike_vars$sample_proportion <- bike_vars$y/bike_vars$n
    bike_vars$posterior_mean <- bike_vars$y_plus_1/bike_vars$n_plus_2
  })
  
  #--- Data
  # Generate data
  bike_df <- reactive({
    # generate data in dataframe
    generateBikeData(sample_size_n = bike_vars$n, probability_theta = bike_vars$theta)
  })
  
  # Display plot description
  output$bike_plot_description <- renderText({
    paste("The plot shows the result of a random sample drawn from a Bernouilli distribution with sample size \\(n\\) =", bike_vars$n, "and true probability of ownership \\(\\theta\\) =", bike_vars$theta)
  })
  
  # Count and display sample size
  output$bike_binom_num_trials <- renderText({
    paste("Sample size:  n =", bike_vars$n)
  })
  
  # Count and display number of yeses
  output$bike_binom_num_yes_responses <- renderText({
    paste("Number of yes responses in n observations:  y =", bike_vars$y)
  })
  
  # Display dotplot of data
  output$bike_dotplot <- renderPlot({
    plotBikeData(df = bike_df(), sample_size_n = bike_vars$n, num_yes_y = bike_vars$y)
  })
  
  #--- Likelihood Distribution
  # Display binomial distribution formula
  output$bike_binom_sampling_dist <- renderUI({ 
    p(withMathJax(sprintf("Likelihood function: \\(p(y=%d | \\theta) = \\binom{%d}{%d} \\theta^{%d}(1-\\theta)^{%d -%d} \\)", 
                          bike_vars$y,
                          bike_vars$n,
                          bike_vars$y,
                          bike_vars$y,
                          bike_vars$n,
                          bike_vars$y)))
  })
  
  # Display binomial distribution plot
  output$bike_binom_sampling_distplot <- renderPlot({
    plotBinomialDist(sample_size_n = bike_vars$n, sample_proportion = bike_vars$sample_proportion)
  })
  
  #--- POSTERIOR DISTRIBUTION
  # Display posterior distribution formula
  output$bike_posterior_dist <- renderUI({ 
    p(withMathJax(sprintf("Posterior distribution: \\(p(\\theta | y=%d) = p(\\theta) p(y=%d | \\theta) \\propto \\theta^{%d}(1-\\theta)^{%d -%d} \\)", 
                          bike_vars$y,
                          bike_vars$y,
                          bike_vars$y,
                          bike_vars$n,
                          bike_vars$y)))
  })
  
  # Display posterior distribution plot
  output$bike_beta_distplot <- renderPlot({
    plotBetaDist(sample_size_n = bike_vars$n, num_yes_y = bike_vars$y)
  })
  
  #--- ESTIMATES
  # Display sample proportion
  output$bike_sample_proportion <- renderUI({ 
    p(withMathJax(sprintf("Sample proportion: \\(\\frac{y}{n} = \\frac{%d}{%d} \\)", 
                          bike_vars$y,
                          bike_vars$n)))
  })
  
  # Display posterior mean
  output$bike_posterior_mean <- renderUI({
    p(withMathJax(sprintf("Posterior mean: \\(\\frac{y + 1}{n + 2} = \\frac{%d}{%d}\\)",
                          bike_vars$y_plus_1, 
                          bike_vars$n_plus_2)))
  })
  
  # Plot estimates
  output$bike_estimates <- renderPlot({
    plotBikeEstimates(prior_mean = bike_vars$prior_mean, 
                      posterior_mean = bike_vars$posterior_mean, 
                      sample_proportion = bike_vars$sample_proportion)
  })
  
  
  ###--- PREELECTION POLLING EXAMPLE ---########################################
  #--- More Global Variables
  # Initialize and keep track of current variable values
  election_vars <- reactiveValues(n = 50,
                                  y1 = 25,
                                  y2 = 20,
                                  y3 = 5,
                                  theta1 = 25/50,
                                  theta2 = 20/50,
                                  theta3 = 5/50,
                                  num_draws = 1000,
                                  median = NULL,  # median of support difference in simulations
                                  quantiles = NULL,  # 5th and 95th quantile of support difference in simulations
                                  posterior_prob1 = NULL,  # estimated posterior probability that candidate 1 has more support than candidate 2
                                  posterior_prob2 = NULL  # est. posterior prob. that candidate 2 has more support than 1
  )
  
  # update global variables and slider maximums with current user input
  observe({
    election_vars$n <- input$election_n
    election_vars$y1 <- input$election_y1
    election_vars$y2 <- input$election_y2
    election_vars$y3 <- input$election_n - input$election_y1 - input$election_y2
    election_vars$voc <- c(election_vars$y1, election_vars$y_2, election_vars$y_3)
    election_vars$num_draws <- input$election_simulation_draws
    
    # Calculate theta
    election_vars$theta1 <- input$election_y1/input$election_n
    election_vars$theta2 <- input$election_y2/input$election_n
    election_vars$theta3 <- election_vars$y3/input$election_n
    
    # Update maximums on sliders
    updateNumericInput(inputId = "election_y1", max = input$election_n)
    updateNumericInput(inputId = "election_y2", max = input$election_n)
  })
  
  # Update global variables when multiSimulationButton is clicked
  observeEvent(input$multiSimulationButton, {
    df <- election_df()
    election_vars$median <- median(df$support_difference)
    election_vars$quantiles <- quantile(df$support_difference, probs = c(0.025, 0.975))
    election_vars$posterior_prob1 <- 100*length(df$support_difference[df$support_difference > 0])/election_vars$num_draws
    election_vars$posterior_prob2 <- 100*length(df$support_difference[df$support_difference < 0])/election_vars$num_draws
  })
  
  # Display no opinion
  output$election_y3 <- renderUI({
    p(withMathJax(sprintf("%d", election_vars$y3)))
  })
  
  # Display vector of counts
  output$election_voc <- renderUI({
    p(withMathJax(sprintf("\\(y=(%d, %d, %d)\\)", election_vars$y1, election_vars$y2, election_vars$y3)))
  })
  
  # Display theta1
  output$election_theta1 <- renderUI({
    p(withMathJax(sprintf("Proportion of support for Candidate 1: \\( \\hat{\\theta}_1 =  %.03f\\)", election_vars$theta1)))
  })
  
  # Display theta2
  output$election_theta2 <- renderUI({
    p(withMathJax(sprintf("Proportion of support for Candidate 2: \\(\\hat{\\theta}_2 =  %.03f\\)", election_vars$theta2)))
  })
  
  # Display theta3
  output$election_theta3 <- renderUI({
    p(withMathJax(sprintf("Proportion of no opinion: \\(\\hat{\\theta}_3 =  %.03f\\)", election_vars$theta3)))
  })
  
  # Display Multinomial Sampling Distribution
  output$election_sampling_dist <- renderUI({
    p(withMathJax(sprintf("\\(p(y |\\hat{\\theta}_1 = %0.02f, \\hat{\\theta}_2 = %0.02f, \\hat{\\theta}_3 = %0.02f) \\propto  (%0.02f)^{y_1} (%0.02f)^{y_2} (%0.02f)^{y_3}  \\)",
                          election_vars$theta1, election_vars$theta2, election_vars$theta3, election_vars$theta1, election_vars$theta2, election_vars$theta3)))
  })
  
  # Display Likelihood Function
  output$election_likelihood_func <- renderUI({
    p(withMathJax(sprintf("\\(p(y_1 = %d, y_2 = %d, y_3 = %d | \\theta) \\propto  \\theta_1^{%d} \\theta_2^{%d}  \\theta_3^{%d}  \\)",
                          election_vars$y1, election_vars$y2, election_vars$y3, election_vars$y1, election_vars$y2, election_vars$y3)))
  })
  
  # Display Prior Distribution
  output$election_prior_dist <- renderUI({
    p(withMathJax(sprintf("\\(p(\\theta) \\propto  \\theta_1^{\\alpha_1 - 1} \\theta_2^{\\alpha_2 -1}  \\theta_3^{\\alpha_3 -1}\\) 
                              \n  with \\(\\alpha_1 = \\alpha_2 = \\alpha_3 = 1  \\)")))
  })
  
  # Display Posterior Dirichlet
  output$election_posterior_dirichlet <- renderUI({
    p(withMathJax(sprintf("\\( \\theta | y\\) ~ \\( Dirichlet(%d, %d, %d) \\)", 
                          election_vars$y1+1, election_vars$y2+1 , election_vars$y3+1)))
  })
  
  # Display Posterior Distribution
  output$election_posterior_dist <- renderUI({
    p(withMathJax(sprintf("\\(p(\\theta | y_1 = %d, y_2 = %d, y_3 = %d) \\propto 
                              \\theta_1^{%d} \\theta_2^{%d}  \\theta_3^{%d}\\)", 
                          election_vars$y1, election_vars$y2, election_vars$y3, election_vars$y1+1, election_vars$y2+1, election_vars$y3+1)))
  })
  
  # Run Simulation
  election_df <- eventReactive(input$multiSimulationButton, {
    runElectionSimulations(election_vars$y1, election_vars$y2, election_vars$y3, election_vars$num_draws)
  })
  
  # Display first 6 simulations
  output$election_simulation_table <- renderTable({
    head(election_df())
  })
  
  # Histogram of support difference
  output$election_simulation_hist <- renderPlot({
    plotElectionSupportDifference(df = election_df(), median = election_vars$median)
  })
  
  # Display support difference median
  output$election_simulation_median <- renderUI({
    p(withMathJax(sprintf("Median: \\(%0.03 f\\)", election_vars$median)))
  })
  
  # Credible set
  output$election_simulation_credible_set <- renderUI({
    p("95% credible set:", withMathJax(sprintf("(%0.03f, %0.03f)", election_vars$quantiles[1], election_vars$quantiles[2] )))
  })
  
  output$election_simulation_posterior_prob1 <- renderUI({
    p(withMathJax(sprintf("Estimated posterior probability that Candidate 1 has more support than 
                              Candidate 2: \\(%0.03f\\)%%", election_vars$posterior_prob1)))
  })
  
  output$election_simulation_posterior_prob2 <- renderUI({
    p(withMathJax(sprintf("Estimated posterior probability that Candidate 2 has more support than 
                              Candidate 1: \\(%0.03f\\)%%", election_vars$posterior_prob2)))
  })
  
  
  ###--- BIOASSAY EXAMPLE ---########################################
  
  # Create data
  bio_df <- reactive({
    # NOTE: doesn't react to any inputs
    xi <- c(-0.86, -0.30, -0.05, 0.73)
    ni <- c(5, 5, 5, 5)
    yi <- c(0, 1, 3, 5)
    df <- data.frame("xi" = xi, "ni" = ni, "yi" = yi)
  })
  
  # Display data
  output$bio_df <- renderTable({
    df <- bio_df()
    # give 'nice' column names
    df %>% dplyr::rename("Dose, xi (log g/ml)" = "xi",
                         "Number of animals, ni" = "ni",
                         "Number of deaths, yi" = "yi")
  })
})
