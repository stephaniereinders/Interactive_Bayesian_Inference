#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    titlePanel("Single Parameter Model"),
    withMathJax(),
    
    
    sidebarLayout(
        sidebarPanel(
            h4("Data"),
            helpText("Our data consists of the results of a sequence of Bernoulli trials. 
                     The result of each trial is either success or failure. We will use 
                     the data to estimate the true proportion \\(\\theta\\) of sucesses in the population."),
            sliderInput("observations",
                        "Number of observations:",
                        min = 1,
                        max = 50,
                        value = 10),
            hr(),
            
            h4("Binomial Sampling Model"),
            textOutput("binom_num_trials"),
            textOutput("binom_num_sucesses"),
            uiOutput("binom_sampling_dist")
            
        ),

        mainPanel(
            plotOutput("dotplot")
        )
    )
))
