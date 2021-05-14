library(shiny)
library(tidyverse)

shinyServer(function(input, output) {
    
    #--- DATA
    # Initialize and keep track of user input
    bernoulli <- reactiveValues(num_obs = 10, num_trials = 1, theta = 0.75)
    
    # Generate data
    df <- reactive({
        # update
        bernoulli$num_obs <- input$observations
        
        # Draw observations from a Bernouilli distribution.
        # Binomial distribution: p(x) = choose(n, x) p^x (1-p)^(n-x)
        # Bernouilli distribution = Binomial distribution with n=1: p(x) = p^x(1-p)^(1-x)
        observations <- stats::rbinom(n=bernoulli$num_obs, size=bernoulli$num_trials, prob=bernoulli$theta)
        observations <- as.factor(unlist(lapply(observations, function(x) if (x==1){"sucess"} else {"failure"})))
        data.frame("observations" = observations)
    })
    
    # Count and display number of trials
    output$binom_num_trials <- renderText({
        # update
        binomial$num_trials <- bernoulli$num_obs
        # display text
        paste("Number of trials:  n =", binomial$num_trials)
    })
    
    # Count and display number of successes
    output$binom_num_successes <- renderText({
        # update
        binomial$num_successes <- dplyr::count(df(), observations)["n"][2,]
        # display text
        paste("Number of successes in n trials:  y =", binomial$num_successes)
    })
    
    # Display sample proportion
    output$sample_proportion <- renderUI({ 
        p(withMathJax(sprintf("Sample proportion: \\(\\frac{y}{n} = \\frac{%d}{%d} \\)", 
                              binomial$num_successes,
                              binomial$num_trials)))
    })
    
    # Display dotplot of data
    output$dotplot <- renderPlot({
        ymax <- max(dplyr::count(df(), observations)["n"])
        ggplot(data=df(), aes(observations)) + 
            geom_dotplot(binwidth = 1/bernoulli$num_obs) +  # make each dot 1/num_obs in diameter
            theme_bw() + 
            coord_fixed(ylim = c(0, ymax/bernoulli$num_obs)) +  # fix y-axis
            scale_y_continuous(NULL, breaks=NULL)  # hide y-axis labels
    })
    
    
    #--- LIKELIHOOD DISTRIBUTION
    # Initialize and keep track of current values (cacluated from user input)
    binomial <- reactiveValues(num_trials = 10,
                               num_successes = 5)
    
    # Display binomial distribution formula
    output$binom_sampling_dist <- renderUI({ 
        p(withMathJax(sprintf("Likelihood distribution: \\(p(y=%d | \\theta) = \\binom{%d}{%d} \\theta^{%d}(1-\\theta)^{%d -%d} \\)", 
                                binomial$num_successes,
                                binomial$num_trials,
                                binomial$num_successes,
                                binomial$num_successes,
                                binomial$num_trials,
                                binomial$num_successes)))
    })
    
    # Display binomial distribution plot
    output$binom_sampling_distplot <- renderPlot({
        successes <- seq(0, binomial$num_trials+2, by = 1)
        probability <- dbinom(successes, size=binomial$num_trials, prob=(binomial$num_successes/binomial$num_trials))
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
                              binomial$num_successes,
                              binomial$num_successes,
                              binomial$num_successes,
                              binomial$num_trials,
                              binomial$num_successes)))
    })
    
    # Display posterior distribution plot
    output$beta_distplot <- renderPlot({
        ggplot() +
            geom_function(fun = function(x) (x^binomial$num_successes)*(1-x)^(binomial$num_trials - binomial$num_successes)) +
            theme_bw()
    })
    
    # Display posterior
    output$posterior_mean <- renderUI({

        numerator <- binomial$num_successes + 1
        denominator <- binomial$num_trials + 2

        p(withMathJax(sprintf("Posterior mean: \\(\\frac{y + 1}{n + 2} = \\frac{%d}{%d}\\)",
                              numerator, denominator)))
    })
    

})
