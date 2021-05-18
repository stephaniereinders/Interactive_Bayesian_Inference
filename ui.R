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
            
            h4("Estimating the Proportion of Bicycle Owners"),
            helpText("Suppose we want to estimate the proportion \\(\\theta\\) of Ames residents that own a bicycle. 
                    Let's make several assumptions in order to simplify this problem."),
            helpText("Assumption 1: the probability of bicycle ownership is exactly the same for each resident."),
            helpText("Assumption 2: whether a resident owns a bicycle is independent of whether any other resident owns a bicycle."),
            helpText("We could ask each resident in a sample of \\(n\\) Ames residents whether or not they own a dog. 
                    Two outcomes are possible, either 'Yes' or 'No.' Let \\(x_i\\) be the response of resident \\(i\\). 
                    Our data \\(x_1, x_2,...,x_n\\) can be thought of as a sequence of Bernouilli trials because of the assumptions 
                    that we made."),
            helpText("Our end goal is to estimate the proportion \\(\\theta\\) of residents that own a bike. We could let \\(y\\) be the number of bicycle owners
                     in our sample, and then use the sample proportion \\(y/n\\) to estimate \\(\\theta\\). But Bayesian inference will give us a 
                     (better? more accurate?) estimate."),
            
            h4("Generate Simulated Data"),
            helpText("In order to illustrate how Bayesian inference works on different data, let's simulate the results of asking a sample of \\(n\\) 
                     Ames residents whether they own a bicycle by performing \\(n\\) Bernouilli trials. This Shiny App uses a built in statistical function 
                     to draw random samples from a Bernouilli distribution. We need to tell this function the sample size \\(n\\) and the (true) probability 
                     \\(\\theta\\) that a resident owns a bicycle. It's important to note that because we are generating the data ourselves we know the 
                     true probability \\(\\theta\\), but in practice when we perfom Bayesian inference we would not know the true probability."),
            
            helpText("Use the sliders below to generate data for various sample sizes and probabilities of dog ownership."),
            sliderInput(inputId = "observations",
                        label = "Sample size \\(n\\):",
                        min = 1,
                        max = 50,
                        value = 10),
            sliderInput(inputId = "theta",
                        label = "True probability of dog ownership \\(\\theta\\):",
                        min = 0,
                        max = 1,
                        value = 0.5),
        ),  # end sidebarPanel
        
        mainPanel(
            tabsetPanel(
                tabPanel("Simulated Data",
                         plotOutput("dotplot"),
                         textOutput("binom_num_trials"),
                         textOutput("binom_num_successes")),
                tabPanel("Estimate Parameter",
                         
                         
                         h4("Prior Distribution"),
                         helpText("We assume that the prior distribution is the uniform distribution on the interval [0,1]"),
                         helpText("Prior distribution: \\(p(\\theta) = 1\\)"),
                         hr(),
                         
                         h4("Likelihood Distribution"),
                         helpText("We summarize our data by considering the total number
                     of successes \\(y\\) in the \\(n\\) trials. (We assume that the \\(n\\) trials are conditionally
                     independent given \\(\\theta\\).)"),
                         uiOutput("binom_sampling_dist"),
                         plotOutput("binom_sampling_distplot"),
                         hr(),
                         
                         h4("Posterior Distribution"),
                         helpText("The unnormalized posterior distribution is prior distribution multiplied by the 
                            likelihood distribution. The posterior distribution takes the form of an unnormalized 
                            beta distribution."),
                         uiOutput("posterior_dist"),
                         plotOutput("beta_distplot"),
                         
                         h4("Estimates of \\(\\theta\\)"),
                         uiOutput("sample_proportion"),
                         helpText("Prior mean: \\(\\frac{1}{2}\\)"),
                         uiOutput("posterior_mean"),
                         plotOutput("estimates"))
            )  # end tabsetPanel
        )  # end mainPanel
    ),  # end sidebarLayout
  )  # end fluidPage
)  # end shinyUI
