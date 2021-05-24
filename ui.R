library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage("Bayesian Data Analysis",
    
    ###--- BICYCLE OWNERSHIP TAB ---########################################    
    tabPanel("Bicycle Ownership Example",

        titlePanel("Estimating the Proportion of Bicycle Owners"),
        p("This example is based on an example of estimating female birth in Bayesian Data Analysis 3rd Edition by Chapman and Hall."),
        withMathJax(),
        
        #--- SIDEBAR    
        sidebarLayout(
            sidebarPanel(
                p("Suppose we want to estimate the proportion \\(\\theta\\) of Ames residents that own a bicycle. 
                        Let's make several assumptions in order to simplify this problem."),
                p(strong("Assumption 1:"), "the probability of bicycle ownership is exactly the same for each resident."),
                p(strong("Assumption 2:"), "whether a resident owns a bicycle is independent of whether any other resident owns a bicycle."),
                p("We could ask each resident in a sample of \\(n\\) Ames residents whether or not they own a dog. 
                        Two outcomes are possible, either 'Yes' or 'No.' Let \\(x_i\\) be the response of resident \\(i\\). 
                        Our data \\(x_1, x_2,...,x_n\\) can be thought of as a sequence of Bernouilli trials because of the assumptions 
                        that we made."),
                p("Our end goal is to estimate the proportion \\(\\theta\\) of residents that own a bike. We could let \\(y\\) be the number of bicycle owners
                         in our sample, and then use the sample proportion \\(y/n\\) to estimate \\(\\theta\\). But Bayesian inference will give us a 
                         (better? more accurate?) estimate."),
                
                h4("Generate Simulated Data"),
                p("In order to illustrate how Bayesian inference works on different data, let's simulate the results of asking a sample of \\(n\\) 
                         Ames residents whether they own a bicycle by performing \\(n\\) Bernouilli trials. This Shiny App uses a built in statistical function 
                         to draw random samples from a Bernouilli distribution. We need to tell this function the sample size \\(n\\) and the (true) probability 
                         \\(\\theta\\) that a resident owns a bicycle. It's important to note that because we are generating the data ourselves we know the 
                         true probability \\(\\theta\\), but in practice when we perfom Bayesian inference we would not know the true probability."),
                
                p("Use the sliders below to generate data for various sample sizes and probabilities of bicycle ownership."),
                sliderInput(inputId = "bike_n",
                            label = "Sample size \\(n\\):",
                            min = 1,
                            max = 50,
                            value = 10),
                sliderInput(inputId = "bike_theta",
                            label = "True probability of dog ownership \\(\\theta\\):",
                            min = 0,
                            max = 1,
                            value = 0.5),
            ),  # end sidebarPanel
            
            #--- MAIN PANEL
            mainPanel(
                tabsetPanel(
                    tabPanel("Simulated Data",
                             plotOutput("bike_dotplot"),
                             textOutput("binom_num_trials"),
                             textOutput("binom_num_yes_responses")),
                    
                    tabPanel("Estimate Proportion of Bicycle Owners",
                             
                             p("We will use Bayesian inference on our simulated data to estimate \\(\\theta\\)."),
                             
                             h4("Prior Distribution"),
                             p("(The prior distribution allows to account for prior information about \\(\\theta\\)?)
                                      Let's use the uniform distribution on the interval [0,1] as the prior distribution \\(p(\\theta)\\)."),
                             p("Prior distribution: \\(p(\\theta) = 1\\)"),
                             hr(),
                             
                             h4("Likelihood Distribution"),
                             p("We can use a binomial distribution to model our data (because of the assumptions we made?)."),
                             uiOutput("bike_binom_sampling_dist"),
                             plotOutput("bike_binom_sampling_distplot"),
                             hr(),
                             
                             h4("Posterior Distribution"),
                             p("The unnormalized posterior distribution is the prior distribution multiplied by the 
                                likelihood distribution. The posterior distribution takes the form of an unnormalized 
                                beta distribution."),
                             uiOutput("bike_posterior_dist"),
                             plotOutput("bike_beta_distplot"),
                             
                             h4("Estimates of \\(\\theta\\)"),
                             p("We could use the sample proportion to estimate the true proportion \\(\\theta\\) of bicycle owners. 
                                      (This estimate depends entirely on the data and does not use any prior information that we incorporated in the
                                      prior distribution?) On the other hand we could use the mean of the prior distribution to estimate \\(\\theta\\), 
                                      but this doesn't use the data at all. The mean of the posterior distribution relies on the data and the prior distribution,
                                      so it provides a (better?) estimate of \\(\\theta\\)."),
                             uiOutput("bike_sample_proportion"),
                             p("Prior mean: \\(\\frac{1}{2}\\)"),
                             uiOutput("bike_posterior_mean"),
                             plotOutput("bike_estimates"))
                )  # end tabsetPanel
            )  # end mainPanel
        )  # end sidebarLayout
    ),  # end tabPanel
    
    
    ###--- PRE-ELECTION POLLING TAB ---######################################## 
    tabPanel("Pre-Election Polling Example",
             
        titlePanel("Pre-Election Polling"),
        p("This example is based on an example in Bayesian Data Analysis 3rd Edition by Chapman and Hall."),
        withMathJax(),
        
        #--- Sidebar
        sidebarLayout(
            sidebarPanel(
                p("Suppose a survey is conducted of \\(n\\) adults in the US during a presidential election year. All responses are grouped into
                one of three categories: supports Candidate 1; supports Candidate 2; or expressed no opinion or supports another candidate. We will use the survey responses to estimate the 
                true proportions \\(\\theta_1, \\theta_2\\) and \\(\\theta_3\\) of these three categories in the population. In particular we want to 
                esitmate the difference in support \\(\\theta_1 - \\theta_2\\) for Candidate 1 and Candidate 2. "),
                hr(),
                
                h4("Survey Responses"),
                numericInput(inputId = "election_n", label = "Sample size", 
                             min = 5, max = 5000, value = 1447, step = 1),
                numericInput(inputId = "election_y1", label = "Supports Candidate 1",
                            min = 0, max = 1447, value = 727, step = 1),
                numericInput(inputId = "election_y2", label = "Supports Candidate 2", 
                            min = 0, max = 1447, value = 583, step = 1),
                p(strong("No Opinion")),
                uiOutput("election_y3"),
                helpText("Adjust candidate values so that 'No Opnion' is not a negative number."),
                p(strong("Vector of Counts")),
                uiOutput("election_voc"),
                hr(),
                
                h4("Sample Proportions"),
                uiOutput("election_theta1"),
                uiOutput("election_theta2"),
                uiOutput("election_theta3"),
                hr(),
                
                h4("Multinomial Sampling Distribution"),
                p("The vector of counts \\(y\\) follows a multinomial distribution for the given sample proportions \\(\\hat{\\theta}\\)"),
                uiOutput("election_sampling_dist"),
                hr(),
                
                h4("Likelihood Function"),
                p("The true proportions \\(\\theta\\) follow a multinomial distribution for the given vector of counts \\(y\\)."),
                uiOutput("election_likelihood_func"),
                hr(),
                
                h4("Non-informative Uniform Prior Distribution"),
                p("We use a Dirichlet distribution as a non-informative uniform prior distribution."),
                uiOutput("election_prior_dist"),
                hr(),
                
                h4("Posterior Distribution"),
                uiOutput("election_posterior_dirichlet"),
                uiOutput("election_posterior_dist"),
                hr(),
                
                h4("Simulations"),
                p("Draw points from the Dirichlet posterior distribution and calculate the support difference \\(\\theta_1 - \\theta_2\\)
                  from each point."),
                sliderInput(inputId = "election_simulation_draws", label = "Draws", min = 500, max = 5000, value= 1000, step = 500),
                actionButton(inputId = "multiSimulationButton", label = "Run Simulations")
            ),
          
            mainPanel(
                h3("First 6 Simulations out of 1000"),
                tableOutput("election_simulation_table"),
                
                h3("Difference in Support (theta1 - theta2) in 1000 Simulations"),
                h4("Histogram"),
                plotOutput("election_simulation_hist"),
                
                h4("Median"),
                uiOutput("election_simulation_median"),
                
                h4("Quantiles"),
                uiOutput("election_simulation_quantile5"),
                uiOutput("election_simulation_quantile95"),
                
                h4("Estimated Posterior Probabilities"),
                uiOutput("election_simulation_posterior_prob1"),
                uiOutput("election_simulation_posterior_prob2")
            )  # end mainPanel 
        )  # end sidebarLayout
    )  # end tabPanel
  )  # end navbarPage
)  # end shinyUI
