#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(R2jags)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("MARE"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "logalpha",label =  "Alpha parameter: ", min = -10, max = 10, value = -10, step=0.01),
            sliderInput(inputId = "logbeta", label = "Beta parameter: ", min = -10, max = 10, value = 1.387, step=0.01)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("PosteriorPlot")
        )
    )
))
