#Load the Library
library(shiny)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(httr)
library(RCurl)

covid <-read.csv(text=getURL("https://raw.githubusercontent.com/vcallier/test/master/inflating%20for%20lags2.csv"), header=T)
covid$date <- dmy(covid$date)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Covid case reporting lags"),
    h4("Covid cases reported in Ontario, Canada from January 21 - March 31."),  
    h5("The downward trend of cases in recent days could be due to incomplete reporting, according to epidemiologist David Fishman. As a result, it may seem like the curve is flattening even though the number of cases continues to increase. See further explanation and data from Dr. Fishman's twitter thread: https://twitter.com/DFisman/status/1245692708376784896"),
    h5("The black dots represent reported Covid cases in Ontario. The red dots show the estimated number of cases, adjusted for the reporting delays. We do not know the average reporting delay. Use the slider to see the adjusted number of cases, depending on the average reporting delay (in days)."),
    
    # Sidebar layout
    sidebarLayout(
        #Inputs: Select which inputs from the data we want to display
        sidebarPanel(
            #Select variable for y-axis
            sliderInput("slider1", h3("Average Delay in Reporting Deaths (in days)"),
                        min = 0, max = 12, value = 5)
        ),
        
        #Output: Type of plot
        mainPanel(
            plotOutput(outputId = "Covid_Cases") #Any name can go where "FreqTab" is, but be sure to keep it consistent with name in output$FreqTab in the server section
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$Covid_Cases <- renderPlot({
        # draw the histogram with the specified number of bins
        ggplot(data = covid)+
        coord_cartesian(xlim=c(dmy("21-01-2020"), dmy("31-03-2020")), ylim= c(0,275))+ 
        
        geom_point(mapping = aes(x = date, y = case), color = "black")+
        geom_smooth(mapping = aes(x = date, y = case), color = "black")+
        
        geom_point(mapping = aes(x = date, y = case/(1-exp((-1/input$slider1)*(50-days)))), color = "red")+
        geom_smooth(mapping = aes(x = date, y = case/(1-exp((-1/input$slider1)*(50-days)))), color = "red")
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)