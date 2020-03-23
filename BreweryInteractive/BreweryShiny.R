#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggthemes)
library(caret)
library(mvtnorm)
library(class)
library(e1071)
library(usmap)

library(rsconnect)

ui <- fluidPage(
    
    # App title ----
    titlePanel("Brewery Study Interactive"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            radioButtons("graphType", "Graph type:",
                         c("Histogram" = "hist",
                           "Boxplot" = "box")),
            # Input: Slider for the number of bins ----
            sliderInput(inputId = "bins",
                        label = "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Histogram ----
            plotOutput(outputId = "distPlot")
            
        )
        
    ),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            radioButtons("graphTypeIBU", "Graph type:",
                         c("Histogram" = "hist",
                           "Boxplot" = "box")),
            # Input: Slider for the number of bins ----
            sliderInput(inputId = "binsIBU",
                        label = "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            # Output: Histogram ----
            plotOutput(outputId = "distPlotIBU")
        )
        
    )
)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
    
    beers <- read.csv("Beers.csv",header = TRUE)
    
    breweries <- read.csv("Breweries.csv",header = TRUE)
    
    # Histogram of the ABV
    output$distPlot <- renderPlot({
        beerABV = beers %>% filter(!is.na(beers$ABV))
        x    <- beerABV$ABV
        if(input$graphType == 'hist'){
            bins <- seq(min(x), max(x), length.out = input$bins + 1)
            hist(x, breaks = bins, col = "#75AADB", border = "white",
                 xlab = "Alchol By Volume",
                 main="Count of Beers per ABV")
        } else {
            boxplot(x,main="Beers by ABV", 
                    ylab="Alcohol by Volume" )
        }
        
        
    })
    output$distPlotIBU <- renderPlot({
        beerIBU = beers %>% filter(!is.na(beers$IBU))
        x    <- beerIBU$IBU
        
        if(input$graphTypeIBU == 'hist'){
            bins <- seq(min(x), max(x), length.out = input$binsIBU + 1)
            
            hist(x, breaks = bins, col = "#75AADB", border = "white",
                 xlab = "International Bitterness Units",
                 main="Count of Beers in IBUs")
        }else {
            boxplot(x,main="Beers by IBU", 
                    ylab="International Bitterness Units" )
        }
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)

rsconnect::setAccountInfo(name='julia-codes',
                          token='8283C342DE7A19745B86143FCE3AC3BD',
                          secret='<SECRET>')
rsconnect::deployApp('BreweryInteractive')

