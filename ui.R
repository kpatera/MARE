# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("iMAREs - interactive Meta analysis of Rare Event studies"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        
        sidebarPanel(width = 2,
            
            selectInput(inputId = 'example', choices=c("Example 1","Example 2","Example 3","Example 4","Example 5"), label = 'Select an example', selected = "Example 1")
            ),
        
        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("Example", 
                                                 tableOutput('exampleOut')),
                              tabPanel("Analysis", 
                                       checkboxGroupInput(inline = TRUE,"checkMethod", label = h3("Select an approach"), 
                                                          choices = list("dl","Mantel","Peto","exact","GLMMLaplace","BayesHN","BayesLogU"),selected = "dl"),
                                       
                                       tableOutput('analysisOut')),
                              tabPanel("PriorPosteriorLogU", 
                                       column(5,
                                       sliderInput(inputId = "logalpha",label =  "Alpha parameter (LogU): ", min = -10, max = 10, value = -10, step=0.01),
                                       sliderInput(inputId = "logbeta", label = "Beta parameter(LogU): ", min = -10, max = 10, value = 1.387, step=0.01),
                                       sliderInput(inputId = "prec.tau",label =  "Precision Tau (HN): ", min = 0, max = 5, value = 2, step=0.1),
                                       sliderInput(inputId = "prec.theta", label = "Precision Theta (Both): ", min = 0, max = 5, value = 0.01 , step=0.001),
                                       sliderInput(inputId = "niter", label = "Number of MCMC iterations (Both): ", min = 5, max = 100000, value = 10000, step=5),
                                       sliderInput(inputId = "nthin", label = "Thinning interval (Both): ", min = 1, max = 50, value = 1, step=1),
                                       ),
                                       column(5,
                                       plotOutput(outputId = "PplotLU",width = "500px",height = "250px"),
                                       plotOutput(outputId = "PplotHN",width = "500px",height = "250px"))
                              ),
                              tabPanel("Examples Details",
                                       "Example 1 : \n Example 2: \n Example 3: \n Example 4: \n Example 5:"
                              )
                              
        )
        )
    )))
