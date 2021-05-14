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
        
        fluidRow(
            column(12,
                   h4("Generate Data"),
                   helpText("Our data consists of the results of a sequence of \\(n\\) (exchangeable) Bernoulli trials. 
                     The result of each trial is either success or failure."),
                   sliderInput("observations",
                               "Number of trials \\(n\\):",
                               min = 1,
                               max = 50,
                               value = 10),
                   textOutput("binom_num_trials"),
                   textOutput("binom_num_successes"),
                   uiOutput("sample_proportion"),
                   hr(),
                   
                   h4("Prior Distribution"),
                   helpText("We assume that the prior distribution is the uniform distribution on the interval [0,1]"),
                   helpText("Prior distribution: \\(p(\\theta) = 1\\)"),
                   helpText("Prior mean: \\(\\frac{1}{2}\\)"),
                   hr(),
                   
                   h4("Likelihood Distribution"),
                   helpText("We summarize our data by considering the total number
                     of successes \\(y\\) in the \\(n\\) trials. (We assume that the \\(n\\) trials are conditionally
                     independent given \\(\\theta\\).)"),
                   uiOutput("binom_sampling_dist"),
                   hr(),
                   
                   h4("Posterior Distribution"),
                   helpText("The unnormalized posterior distribution is prior distribution multiplied by the 
                            likelihood distribution. The posterior distribution takes the form of an unnormalized 
                            beta distribution."),
                   uiOutput("posterior_dist"),
                   uiOutput("posterior_mean"),
                   hr()
            ),  # end column
            
            column(4,
                   h4("Data"),
                   plotOutput("dotplot"),
            ),  # end column
            
            column(4,
                   h4("Likelihood Distribution"),
                   plotOutput("binom_sampling_distplot"),
            ),  # end column
            
            column(4,
                   h4("Posterior Distribution"),
                   plotOutput("beta_distplot")
            ),  # end column
            
            column(6, offset=3,
                   plotOutput("estimates")
                   
                   
            )  # end column
        )  # end fluidRow 
    )  # end fluidPage
)  # end shinyUI
