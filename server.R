library(shiny)
library(tidyverse)
source("R/single_parameter.R")

shinyServer(function(input, output) {
    
    ###--- SINGLE-PARAMETER MODEL ---########################################
    
    #--- Global Variables
    # Initialize and keep track of current variable values
    single_vars <- reactiveValues(theta = 0.75,
                                  n = 10,  # number of trials 
                                  y = 5,  # number of successes
                                  y_plus_1 = 6,  # parameter for beta distribution
                                  n_plus_2 = 12,  # parameter for beta distribution
                                  sample_proportion = 5/10,  # y/n
                                  prior_mean = 1/2, 
                                  posterior_mean = 6/12  # (y+1)/(n+2)
                                  )
    
    #--- Data
    # Generate data
    df <- reactive({
        # update
        single_vars$theta <- input$single_theta
        single_vars$n <- input$single_n
        
        # generate data in dataframe
        generateSingleData(sample_size_n = single_vars$n, probability_theta = single_vars$theta)
    })
    
    # Count and display sample size
    output$binom_num_trials <- renderText({
        paste("Sample size:  n =", single_vars$n)
    })
    
    # Count and display number of yeses
    output$binom_num_yes_responses <- renderText({
        # update global variable
        single_vars$y <- dplyr::count(df(), observations)["n"][2,]
        
        # display text
        paste("Number of yeses in n observations:  y =", single_vars$y)
    })
    
    # Display dotplot of data
    output$dotplot <- renderPlot({
        plotSingleData(df = df(), sample_size_n = single_vars$n, num_yes_y = single_vars$y)
    })
    
    #--- Likelihood Distribution
    # Display binomial distribution formula
    output$binom_sampling_dist <- renderUI({ 
        p(withMathJax(sprintf("Likelihood distribution: \\(p(y=%d | \\theta) = \\binom{%d}{%d} \\theta^{%d}(1-\\theta)^{%d -%d} \\)", 
                                single_vars$y,
                                single_vars$n,
                                single_vars$y,
                                single_vars$y,
                                single_vars$n,
                                single_vars$y)))
    })
    
    # Display binomial distribution plot
    output$binom_sampling_distplot <- renderPlot({
        plotBinomialDist(sample_size_n = single_vars$n, sample_proportion = single_vars$sample_proportion)
    })
    
    #--- POSTERIOR DISTRIBUTION
    # Display posterior distribution formula
    output$posterior_dist <- renderUI({ 
        p(withMathJax(sprintf("Posterior distribution: \\(p(\\theta | y=%d) = p(\\theta) p(y=%d | \\theta) \\propto \\theta^{%d}(1-\\theta)^{%d -%d} \\)", 
                              single_vars$y,
                              single_vars$y,
                              single_vars$y,
                              single_vars$n,
                              single_vars$y)))
    })
    
    # Display posterior distribution plot
    output$beta_distplot <- renderPlot({
        plotBetaDist(sample_size_n = single_vars$n, num_yes_y = single_vars$y)
    })
    
    #--- ESTIMATES
    # Display sample proportion
    output$sample_proportion <- renderUI({ 
        p(withMathJax(sprintf("Sample proportion: \\(\\frac{y}{n} = \\frac{%d}{%d} \\)", 
                              single_vars$y,
                              single_vars$n)))
    })
    
    # Display posterior mean
    output$posterior_mean <- renderUI({
        # Update
        single_vars$y_plus_1 <- single_vars$y + 1
        single_vars$n_plus_2 <- single_vars$n + 2
        
        # Display
        p(withMathJax(sprintf("Posterior mean: \\(\\frac{y + 1}{n + 2} = \\frac{%d}{%d}\\)",
                              single_vars$y_plus_1, 
                              single_vars$n_plus_2)))
    })
    
    # Plot estimates
    output$estimates <- renderPlot({
        # Update
        single_vars$sample_proportion <- single_vars$y/single_vars$n
        single_vars$posterior_mean <- single_vars$y_plus_1/single_vars$n_plus_2
        
        plotSingleEstimates(prior_mean = single_vars$prior_mean, 
                            posterior_mean = single_vars$posterior_mean, 
                            sample_proportion = single_vars$sample_proportion)
    })
    
    
    ###--- MULTIPARAMETER MODEL ---########################################
    #--- More Global Variables
    # Initialize and keep track of current variable values
    multi_vars <- reactiveValues(n = 10
    )
    
    # update multi_vars with current user input
    observe({multi_vars$n <- input$multi_n})
    
    # Display sample size
    output$multi_n <- renderUI({
        p(withMathJax(sprintf("Sample size: \\(%d \\)", 
                              multi_vars$n
                              )))
    })
    
    
    
})
