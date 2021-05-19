library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage("Bayesian Data Analysis",
    
    #--- SINGLE PARAMETER MODEL TAB         
    tabPanel("Single Parameter Model",

        titlePanel("Single Parameter Model"),
        withMathJax(),
        
        #--- SIDEBAR    
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
                sliderInput(inputId = "single_n",
                            label = "Sample size \\(n\\):",
                            min = 1,
                            max = 50,
                            value = 10),
                sliderInput(inputId = "single_theta",
                            label = "True probability of dog ownership \\(\\theta\\):",
                            min = 0,
                            max = 1,
                            value = 0.5),
            ),  # end sidebarPanel
            
            #--- MAIN PANEL
            mainPanel(
                tabsetPanel(
                    tabPanel("Simulated Data",
                             plotOutput("dotplot"),
                             textOutput("binom_num_trials"),
                             textOutput("binom_num_yes_responses")),
                    
                    tabPanel("Estimate Proportion of Bicycle Owners",
                             
                             helpText("We will use Bayesian inference on our simulated data to estimate \\(\\theta\\)."),
                             
                             h4("Prior Distribution"),
                             helpText("(The prior distribution allows to account for prior information about \\(\\theta\\)?)
                                      Let's use the uniform distribution on the interval [0,1] as the prior distribution \\(p(\\theta)\\)."),
                             helpText("Prior distribution: \\(p(\\theta) = 1\\)"),
                             hr(),
                             
                             h4("Likelihood Distribution"),
                             helpText("We can use a binomial distribution to model our data (because of the assumptions we made?)."),
                             uiOutput("binom_sampling_dist"),
                             plotOutput("binom_sampling_distplot"),
                             hr(),
                             
                             h4("Posterior Distribution"),
                             helpText("The unnormalized posterior distribution is the prior distribution multiplied by the 
                                likelihood distribution. The posterior distribution takes the form of an unnormalized 
                                beta distribution."),
                             uiOutput("posterior_dist"),
                             plotOutput("beta_distplot"),
                             
                             h4("Estimates of \\(\\theta\\)"),
                             helpText("We could use the sample proportion to estimate the true proportion \\(\\theta\\) of bicycle owners. 
                                      (This estimate depends entirely on the data and does not use any prior information that we incorporated in the
                                      prior distribution?) On the other hand we could use the mean of the prior distribution to estimate \\(\\theta\\), 
                                      but this doesn't use the data at all. The mean of the posterior distribution relies on the data and the prior distribution,
                                      so it provides a (better?) estimate of \\(\\theta\\)."),
                             uiOutput("sample_proportion"),
                             helpText("Prior mean: \\(\\frac{1}{2}\\)"),
                             uiOutput("posterior_mean"),
                             plotOutput("estimates"))
                )  # end tabsetPanel
            )  # end mainPanel
        ),  # end sidebarLayout
    )  # end tabPanel
    
    
    
  )  # end navbarPage
)  # end shinyUI
