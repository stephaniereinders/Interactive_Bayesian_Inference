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
    
    # Update global variables with current user input
    observe({
        # Update directly from user input
        single_vars$theta <- input$single_theta
        single_vars$n <- input$single_n
        single_vars$n_plus_2 <- single_vars$n + 2  # parameter for beta distribution
        
        # Update df from user input first to then update variable
        single_vars$y <- dplyr::count(df(), observations)["n"][2,]
        single_vars$y_plus_1 <- single_vars$y + 1  # parameter for beta distribution
        single_vars$sample_proportion <- single_vars$y/single_vars$n
        single_vars$posterior_mean <- single_vars$y_plus_1/single_vars$n_plus_2
    })

    #--- Data
    # Generate data
    df <- reactive({
        # generate data in dataframe
        generateSingleData(sample_size_n = single_vars$n, probability_theta = single_vars$theta)
    })
    
    # Count and display sample size
    output$binom_num_trials <- renderText({
        paste("Sample size:  n =", single_vars$n)
    })
    
    # Count and display number of yeses
    output$binom_num_yes_responses <- renderText({
        paste("Number of yes responses in n observations:  y =", single_vars$y)
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
        p(withMathJax(sprintf("Posterior mean: \\(\\frac{y + 1}{n + 2} = \\frac{%d}{%d}\\)",
                              single_vars$y_plus_1, 
                              single_vars$n_plus_2)))
    })
    
    # Plot estimates
    output$estimates <- renderPlot({
        plotSingleEstimates(prior_mean = single_vars$prior_mean, 
                            posterior_mean = single_vars$posterior_mean, 
                            sample_proportion = single_vars$sample_proportion)
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
                                 theta3 = 5/50
    )
    
    # update multi_vars and slider maximums with current user input
    observe({
        multi_vars$n <- input$multi_n
        multi_vars$y1 <- input$multi_y1
        multi_vars$y2 <- input$multi_y2
        multi_vars$y3 <- input$multi_n - input$multi_y1 - input$multi_y2
        multi_vars$voc <- c(multi_vars$y1, multi_vars$y_2, multi_vars$y_3)
        
        # Calculate theta
        multi_vars$theta1 <- input$multi_y1/input$multi_n
        multi_vars$theta2 <- input$multi_y2/input$multi_n
        multi_vars$theta3 <- multi_vars$y3/input$multi_n
        
        # Update maximums on sliders
        updateSliderInput(inputId = "multi_y1", max = input$multi_n)
        updateSliderInput(inputId = "multi_y2", max = input$multi_n)
        })
    
    # Display sample size
    output$multi_n <- renderUI({
        p(withMathJax(sprintf("Sample size: \\(n=%d \\)", multi_vars$n)))
    })
    
    # Display candidate A support
    output$multi_y1 <- renderUI({
        p(withMathJax(sprintf("Supports Candidate A: \\(y_1=%d\\)", multi_vars$y1)))
    })
    
    # Display candidate B support
    output$multi_y2 <- renderUI({
        p(withMathJax(sprintf("Supports Candidate B: \\(y_2=%d\\)", multi_vars$y2)))
    })
    
    # Display no opinion
    output$multi_y3 <- renderUI({
        p(withMathJax(sprintf("No Opinion: \\(y_3=%d\\)", multi_vars$y3)))
    })
    
    # Display vector of counts
    output$multi_voc <- renderUI({
        p(withMathJax(sprintf("Vector of counts: \\(y=(%d, %d, %d)\\)", multi_vars$y1, multi_vars$y2, multi_vars$y3)))
    })
    
    # Display theta1
    output$multi_theta1 <- renderUI({
        p(withMathJax(sprintf("\\(\\theta_1 =  %.03f\\)", multi_vars$theta1)))
    })
    
    # Display theta2
    output$multi_theta2 <- renderUI({
        p(withMathJax(sprintf("\\(\\theta_1 =  %.03f\\)", multi_vars$theta2)))
    })
    
    # Display theta3
    output$multi_theta3 <- renderUI({
        p(withMathJax(sprintf("\\(\\theta_1 =  %.03f\\)", multi_vars$theta3)))
    })
    
    # Display Multinomial Sampling Distribution
    output$multi_sampling_dist <- renderUI({
        p(withMathJax(sprintf("\\(p(y | \\theta_1 = %0.02f, \\theta_2 = %0.02f, \\theta_3 = %0.02f) \\propto  (%0.02f)^{y_1} (%0.02f)^{y_2} (%0.02f)^{y_3}  \\)",
                              multi_vars$theta1, multi_vars$theta2, multi_vars$theta3, multi_vars$theta1, multi_vars$theta2, multi_vars$theta3)))
    })
    
    
    # Display Likelihood Distribution
    output$multi_likelihood_dist <- renderUI({
        p(withMathJax(sprintf("\\(p(y_1 = %d, y_2 = %d, y_3 = %d | \\theta) \\propto  \\theta_1^{%d} \\theta_2^{%d}  \\theta_3^{%d}  \\)",
                              multi_vars$y1, multi_vars$y2, multi_vars$y3, multi_vars$y1, multi_vars$y2, multi_vars$y3)))
    })
    
    # Display Prior Distribution
    output$multi_prior_dist <- renderUI({
        p(withMathJax(sprintf("\\(p(\\theta) \\propto  \\theta_1^{\\alpha_1 - 1} \\theta_2^{\\alpha_2 -1}  \\theta_3^{\\alpha_3 -1} \\ \\ \\  \\ with \\ \\alpha_1 = \\alpha_2 = \\alpha_3 = 1  \\)")))
    })
    
    # Display Posterior Distribution
    output$multi_posterior_dist <- renderUI({
        p(withMathJax(sprintf("\\(p(\\theta | y_1 = %d, y_2 = %d, y_3 = %d) \\propto  p(\\theta) p(y_1 = %d, y_2 = %d, y_3 = %d | \\theta) =
                              \\theta_1^{%d} \\theta_2^{%d}  \\theta_3^{%d}\\)", 
                              multi_vars$y1, multi_vars$y2, multi_vars$y3, multi_vars$y1, multi_vars$y2, multi_vars$y3, multi_vars$y1, multi_vars$y2, multi_vars$y3)))
    })

    
    
})
