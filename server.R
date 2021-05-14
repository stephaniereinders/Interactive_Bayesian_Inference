library(shiny)
library(tidyverse)

shinyServer(function(input, output) {
    
    #--- GLOBAL VARIABLES
    # Initialize and keep track of current variable values
    global_vars <- reactiveValues(theta = 0.75,
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
        global_vars$n <- input$observations
        
        # Draw observations from a Bernouilli distribution.
        # Bernouilli distribution = Binomial distribution with n=1: p(x) = p^x(1-p)^(1-x)
        observations <- stats::rbinom(n = global_vars$n, 
                                      size = 1, 
                                      prob=global_vars$theta)
        observations <- as.factor(unlist(lapply(observations, function(x) if (x==1){"success"} else {"failure"})))
        data.frame("observations" = observations)
    })
    
    # Count and display number of trials
    output$binom_num_trials <- renderText({
        # display text
        paste("Number of trials:  n =", global_vars$n)
    })
    
    # Count and display number of successes
    output$binom_num_successes <- renderText({
        # update
        global_vars$y <- dplyr::count(df(), observations)["n"][2,]
        # display text
        paste("Number of successes in n trials:  y =", global_vars$y)
    })
    
    # Display sample proportion
    output$sample_proportion <- renderUI({ 
        p(withMathJax(sprintf("Sample proportion: \\(\\frac{y}{n} = \\frac{%d}{%d} \\)", 
                              global_vars$y,
                              global_vars$n)))
    })
    
    # Display dotplot of data
    output$dotplot <- renderPlot({
        
        # Make max height proportional to num successes or num failures, whichever is larger. (Each circle will be 1/n in diameter 
        # so total height of successes or failures circles will be y*(1/n) or (n-y)*(1/n)).) 
        num_failures <- global_vars$n - global_vars$y
        if (global_vars$y >= num_failures){
            max_height <- global_vars$y/global_vars$n
        } else {
            max_height <- num_failures/global_vars$n
        }

        ggplot(data=df(), aes(observations)) + 
            geom_dotplot(binwidth = 1/global_vars$n) +  # make each dot 1/num_obs in diameter
            theme_bw() + 
            coord_fixed(ylim = c(0, max_height)) +  # fix y-axis
            scale_y_continuous(NULL, breaks=NULL)  # hide y-axis labels
    })
    
    
    #--- LIKELIHOOD DISTRIBUTION
    # Display binomial distribution formula
    output$binom_sampling_dist <- renderUI({ 
        p(withMathJax(sprintf("Likelihood distribution: \\(p(y=%d | \\theta) = \\binom{%d}{%d} \\theta^{%d}(1-\\theta)^{%d -%d} \\)", 
                                global_vars$y,
                                global_vars$n,
                                global_vars$y,
                                global_vars$y,
                                global_vars$n,
                                global_vars$y)))
    })
    
    # Display binomial distribution plot
    output$binom_sampling_distplot <- renderPlot({
        # Generate a sequence of all possible numbers of successes (I.e. 1, 2,...,n). 
        # Let the sequence go up to n+2 to just to make the plot look nicer.
        successes <- seq(0, global_vars$n+2, by = 1)  
        
        # Calculate the probability of obtaining each number of successes in n trials
        probability <- dbinom(successes, size=global_vars$n, prob=(global_vars$y/global_vars$n))
        
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
                              global_vars$y,
                              global_vars$y,
                              global_vars$y,
                              global_vars$n,
                              global_vars$y)))
    })
    
    # Display posterior distribution plot
    output$beta_distplot <- renderPlot({
        ggplot() +
            geom_function(fun = function(x) (x^global_vars$y)*(1-x)^(global_vars$n - global_vars$y)) +
            theme_bw()
    })
    
    # Display posterior
    output$posterior_mean <- renderUI({
        # Update
        global_vars$y_plus_1 <- global_vars$y + 1
        global_vars$n_plus_2 <- global_vars$n + 2
        
        # Display
        p(withMathJax(sprintf("Posterior mean: \\(\\frac{y + 1}{n + 2} = \\frac{%d}{%d}\\)",
                              global_vars$y_plus_1, 
                              global_vars$n_plus_2)))
    })
    
    
    #--- ESTIMATES
    output$estimates <- renderPlot({
        # Update
        global_vars$sample_proportion <- global_vars$y/global_vars$n
        global_vars$posterior_mean <- global_vars$y_plus_1/global_vars$n_plus_2
        
        # Make dataframe
        x <- c(global_vars$sample_proportion, global_vars$posterior_mean, global_vars$prior_mean)
        x_labels <- c("sample proportion", "posterior mean", "prior mean")
        y <- c(0, 0, 0)
        df <- data.frame("x" = x, "x_labels" = x_labels, "y" = y)
        # df$x_labels <- factor(df$x_labels, levels = c("sample proportion", "posterior mean", "prior mean"))
        
        # Plot
        df %>% ggplot(aes(x=x, y=y)) + 
            geom_hline(yintercept=0) + 
            geom_point(size=5, aes(color=x_labels, shape=x_labels)) + 
            theme_bw() + 
            scale_color_manual(values=RColorBrewer::brewer.pal(n=3, name="Dark2")) + 
            coord_fixed(ylim = c(-0.005, 0.005)) +  # fix y-axis
            scale_y_continuous(NULL, breaks=NULL) +  # hide y-axis labels
            scale_x_continuous(NULL) +
            labs(color = "Estimate Type", shape = "Estimate Type") + 
            theme(legend.position = "bottom")
    })
})
