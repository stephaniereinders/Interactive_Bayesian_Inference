library(shiny)
library(tidyverse)

shinyServer(function(input, output) {
    
    bernoulli <- reactiveValues(num_obs = 10,
                                num_trials = 1,
                                theta = 0.75)
    
    binomial <- reactiveValues(num_trials = 10,
                               num_sucesses = 5)
    
    output$binom_num_trials <- renderText({
        # update
        binomial$num_trials <- bernoulli$num_obs
        
        # display text
        paste("Number of trials:  n =", binomial$num_trials)
    })
    
    output$binom_num_sucesses <- renderText({
        # update
        binomial$num_sucesses <- dplyr::count(df(), observations)["n"][2,]
        
        # display text
        paste("Number of sucesses in n trials:  y =", binomial$num_sucesses)
    })
    
    output$binom_sampling_dist <- renderUI({ 
        p(withMathJax(sprintf("$$p(y=%d | \\theta) = \\binom{%d}{%d} \\theta^{%d}(1-\\theta)^{%d -%d} $$", 
                                binomial$num_sucesses,
                                binomial$num_trials,
                                binomial$num_sucesses,
                                binomial$num_sucesses,
                                binomial$num_trials,
                                binomial$num_sucesses)))
    })
    
    output$posterior_dist <- renderUI({ 
        p(withMathJax(sprintf("$$p(\\theta | y=%d) = p(\\theta) p(y=%d | \\theta) \\propto \\theta^{%d}(1-\\theta)^{%d -%d} $$", 
                              binomial$num_sucesses,
                              binomial$num_sucesses,
                              binomial$num_sucesses,
                              binomial$num_trials,
                              binomial$num_sucesses)))
    })
    
    
    #--- Generate data
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
    
    #--- Display dotplot of data
    output$dotplot <- renderPlot({
        ymax <- max(dplyr::count(df(), observations)["n"])
        ggplot(data=df(), aes(observations)) + 
            geom_dotplot(binwidth = 1/bernoulli$num_obs) +  # make each dot 1/num_obs in diameter
            theme_bw() + 
            coord_fixed(ylim = c(0, ymax/bernoulli$num_obs)) +  # fix y-axis
            scale_y_continuous(NULL, breaks=NULL)  # hide y-axis labels
    })
    
    #--- Binomial Plot
    output$binom_sampling_distplot <- renderPlot({
        successes <- seq(0, binomial$num_trials+2, by = 1)
        probability <- dbinom(successes, size=binomial$num_trials, prob=(binomial$num_sucesses/binomial$num_trials))
        df <- data.frame("successes" = successes, "probability" = probability)
        
        # Graph
        ggplot(df, aes(x=successes, y=probability)) + 
            geom_point() +
            theme_bw()
    })
    
    output$beta_distplot <- renderPlot({
        shape1 <- binomial$num_sucesses + 1
        shape2 <- binomial$num_trials - binomial$num_sucesses + 1
        
        ggplot() +
            geom_function(fun = dbeta, args = list(shape1, shape2)) +
            theme_bw()
    })
    

})
