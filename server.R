library(shiny)
library(ggplot2)
library(dplyr)
library(gtools)  # used for Dirichlet distribution
source("R/bicycle_ownership.R")
source("R/multi_parameter.R")

shinyServer(function(input, output) {
    
    ###--- BICYCLE OWNERSHIP MODEL ---########################################
    
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
        p(withMathJax(sprintf("Likelihood distribution: \\(p(y=%d | \\theta) = \\binom{%d}{%d} \\theta^{%d}(1-\\theta)^{%d -%d} \\)", 
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
    
    
    ###--- MULTIPARAMETER MODEL ---########################################
    #--- More Global Variables
    # Initialize and keep track of current variable values
    multi_vars <- reactiveValues(n = 50,
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
        multi_vars$n <- input$multi_n
        multi_vars$y1 <- input$multi_y1
        multi_vars$y2 <- input$multi_y2
        multi_vars$y3 <- input$multi_n - input$multi_y1 - input$multi_y2
        multi_vars$voc <- c(multi_vars$y1, multi_vars$y_2, multi_vars$y_3)
        multi_vars$num_draws <- input$multi_simulation_draws
        
        # Calculate theta
        multi_vars$theta1 <- input$multi_y1/input$multi_n
        multi_vars$theta2 <- input$multi_y2/input$multi_n
        multi_vars$theta3 <- multi_vars$y3/input$multi_n
        
        # Update maximums on sliders
        updateNumericInput(inputId = "multi_y1", max = input$multi_n)
        updateNumericInput(inputId = "multi_y2", max = input$multi_n)
        })
    
    # Update global variables when multiSimulationButton is clicked
    observeEvent(input$multiSimulationButton, {
        df <- multi_df()
        multi_vars$median <- median(df$support_difference)
        multi_vars$quantiles <- quantile(df$support_difference, probs = c(0.05, 0.95))
        multi_vars$posterior_prob1 <- 100*length(df$support_difference[df$support_difference > 0])/multi_vars$num_draws
        multi_vars$posterior_prob2 <- 100*length(df$support_difference[df$support_difference < 0])/multi_vars$num_draws
    })
    
    # Display no opinion
    output$multi_y3 <- renderUI({
        p(withMathJax(sprintf("%d", multi_vars$y3)))
    })
    
    # Display vector of counts
    output$multi_voc <- renderUI({
        p(withMathJax(sprintf("\\(y=(%d, %d, %d)\\)", multi_vars$y1, multi_vars$y2, multi_vars$y3)))
    })
    
    # Display theta1
    output$multi_theta1 <- renderUI({
        p(withMathJax(sprintf("Proportion of support for Candidate 1: \\( \\hat{\\theta}_1 =  %.03f\\)", multi_vars$theta1)))
    })
    
    # Display theta2
    output$multi_theta2 <- renderUI({
        p(withMathJax(sprintf("Proportion of support for Candidate 2: \\(\\hat{\\theta}_2 =  %.03f\\)", multi_vars$theta2)))
    })
    
    # Display theta3
    output$multi_theta3 <- renderUI({
        p(withMathJax(sprintf("Proportion of no opinion: \\(\\hat{\\theta}_3 =  %.03f\\)", multi_vars$theta3)))
    })
    
    # Display Multinomial Sampling Distribution
    output$multi_sampling_dist <- renderUI({
        p(withMathJax(sprintf("\\(p(y |\\hat{\\theta}_1 = %0.02f, \\hat{\\theta}_2 = %0.02f, \\hat{\\theta}_3 = %0.02f) \\propto  (%0.02f)^{y_1} (%0.02f)^{y_2} (%0.02f)^{y_3}  \\)",
                              multi_vars$theta1, multi_vars$theta2, multi_vars$theta3, multi_vars$theta1, multi_vars$theta2, multi_vars$theta3)))
    })
    
    # Display Likelihood Function
    output$multi_likelihood_func <- renderUI({
        p(withMathJax(sprintf("\\(p(y_1 = %d, y_2 = %d, y_3 = %d | \\theta) \\propto  \\theta_1^{%d} \\theta_2^{%d}  \\theta_3^{%d}  \\)",
                              multi_vars$y1, multi_vars$y2, multi_vars$y3, multi_vars$y1, multi_vars$y2, multi_vars$y3)))
    })
    
    # Display Prior Distribution
    output$multi_prior_dist <- renderUI({
        p(withMathJax(sprintf("\\(p(\\theta) \\propto  \\theta_1^{\\alpha_1 - 1} \\theta_2^{\\alpha_2 -1}  \\theta_3^{\\alpha_3 -1}\\) 
                              \n  with \\(\\alpha_1 = \\alpha_2 = \\alpha_3 = 1  \\)")))
    })
    
    # Display Posterior Dirichlet
    output$multi_posterior_dirichlet <- renderUI({
        p(withMathJax(sprintf("\\( \\theta | y\\) ~ \\( Dirichlet(%d, %d, %d) \\)", 
                              multi_vars$y1+1, multi_vars$y2+1 , multi_vars$y3+1)))
    })
    
    # Display Posterior Distribution
    output$multi_posterior_dist <- renderUI({
        p(withMathJax(sprintf("\\(p(\\theta | y_1 = %d, y_2 = %d, y_3 = %d) \\propto 
                              \\theta_1^{%d} \\theta_2^{%d}  \\theta_3^{%d}\\)", 
                              multi_vars$y1, multi_vars$y2, multi_vars$y3, multi_vars$y1+1, multi_vars$y2+1, multi_vars$y3+1)))
    })
    
    # Run Simulation
    multi_df <- eventReactive(input$multiSimulationButton, {
        runSimulations(multi_vars$y1, multi_vars$y2, multi_vars$y3, multi_vars$num_draws)
    })
    
    # Display first 6 simulations
    output$multi_simulation_table <- renderTable({
        head(multi_df())
    })
    
    # Histogram of support difference
    output$multi_simulation_hist <- renderPlot({
        ggplot2::ggplot(multi_df(), aes(support_difference)) +
            geom_histogram() +
            geom_vline(xintercept=0, color="red") +
            theme_bw() 
    })
    
    # Display support difference median
    output$multi_simulation_median <- renderUI({
        p(withMathJax(sprintf("Median: \\(%0.03 f\\)", multi_vars$median)))
    })
    
    # 5th and 95th quantiles
    output$multi_simulation_quantile5 <- renderUI({
        p(withMathJax(sprintf("5th quantile: \\(%f\\)", multi_vars$quantiles[1])))
    })
    
    output$multi_simulation_quantile95 <- renderUI({
        p(withMathJax(sprintf("95th quantile: \\(%f\\)", multi_vars$quantiles[2])))
    })
    
    output$multi_simulation_posterior_prob1 <- renderUI({
        p(withMathJax(sprintf("Estimated posterior probability that Candidate 1 has more support than 
                              Candidate 2: \\(%0.03f\\)%%", multi_vars$posterior_prob1)))
    })
    
    output$multi_simulation_posterior_prob2 <- renderUI({
        p(withMathJax(sprintf("Estimated posterior probability that Candidate 2 has more support than 
                              Candidate 1: \\(%0.03f\\)%%", multi_vars$posterior_prob2)))
    })

    
})
