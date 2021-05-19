library(shiny)
library(tidyverse)
source("R/single_parameter.R")

shinyServer(function(input, output) {
    
    ###--- SINGLE PARAMETER MODEL ---###
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
    
    #--- DATA
    # Generate data
    df <- reactive({
        # update
        single_vars$theta <- input$single_theta
        single_vars$n <- input$single_n
        
        # generate data in dataframe
        generateSingleParameterData(sample_size_n = single_vars$n, probability_theta = single_vars$theta)
    })
    
    # Count and display sample size
    output$binom_num_trials <- renderText({
        # display text
        paste("Sample size:  n =", single_vars$n)
    })
    
    # Count and display number of yeses
    output$binom_num_successes <- renderText({
        # update
        single_vars$y <- dplyr::count(df(), observations)["n"][2,]
        
        # display text
        paste("Number of yeses in n observations:  y =", single_vars$y)
    })
    
    # Display sample proportion
    output$sample_proportion <- renderUI({ 
        p(withMathJax(sprintf("Sample proportion: \\(\\frac{y}{n} = \\frac{%d}{%d} \\)", 
                              single_vars$y,
                              single_vars$n)))
    })
    
    # Display dotplot of data
    output$dotplot <- renderPlot({
        
        makeDataDotplot(df = df(), sample_size_n = single_vars$n, num_yes_y = single_vars$y)

    })
    
    #--- LIKELIHOOD DISTRIBUTION
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
        # Generate a sequence of all possible numbers of successes (I.e. 1, 2,...,n). 
        # Let the sequence go up to n+2 to just to make the plot look nicer.
        successes <- seq(0, single_vars$n+2, by = 1)  
        
        # Calculate the probability of obtaining each number of successes in n trials
        probability <- dbinom(successes, size=single_vars$n, prob=(single_vars$y/single_vars$n))
        
        # Create data frame
        df <- data.frame("successes" = successes, "probability" = probability)
        
        # Graph
        ggplot(df, aes(x=successes, y=probability)) + 
            geom_point() +
            theme_bw()
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
        ggplot() +
            geom_function(fun = function(x) (x^single_vars$y)*(1-x)^(single_vars$n - single_vars$y)) +
            theme_bw()
    })
    
    # Display posterior
    output$posterior_mean <- renderUI({
        # Update
        single_vars$y_plus_1 <- single_vars$y + 1
        single_vars$n_plus_2 <- single_vars$n + 2
        
        # Display
        p(withMathJax(sprintf("Posterior mean: \\(\\frac{y + 1}{n + 2} = \\frac{%d}{%d}\\)",
                              single_vars$y_plus_1, 
                              single_vars$n_plus_2)))
    })
    
    #--- ESTIMATES
    output$estimates <- renderPlot({
        # Update
        single_vars$sample_proportion <- single_vars$y/single_vars$n
        single_vars$posterior_mean <- single_vars$y_plus_1/single_vars$n_plus_2
        
        # Make dataframe
        x <- c(single_vars$prior_mean, single_vars$posterior_mean, single_vars$sample_proportion)
        x_labels <- c("prior mean", "posterior mean", "sample proportion")
        y <- c(0, 0, 0)
        df <- data.frame("x" = x, "x_labels" = x_labels, "y" = y)
        df$x_labels <- factor(df$x_labels, levels = c("prior mean", "posterior mean", "sample proportion"))  # fix order in legend
        
        # Plot
        df %>% ggplot(aes(x=x, y=y)) + 
            geom_hline(yintercept=0) + 
            geom_point(size=5, aes(color=x_labels, shape=x_labels)) + 
            theme_bw() + 
            scale_color_manual(values=RColorBrewer::brewer.pal(n=3, name="Dark2")) + 
            coord_fixed(ylim = c(-0.025, 0.025)) +  # fix y-axis
            scale_y_continuous(NULL, breaks=NULL) +  # hide y-axis labels
            scale_x_continuous(NULL) +
            labs(color = "Estimate Type", shape = "Estimate Type") + 
            theme(legend.position = "bottom")
    })
})
