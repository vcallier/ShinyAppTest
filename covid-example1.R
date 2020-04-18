#Load the Library
library(shiny)
library(ggplot2)
library(tidyverse)
covid <- read_csv("inflating for lags2.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Covid case reporting lags"),
    h4("Covid cases reported in Ontario, Canada from January 21 - March 31."),  
    h5("Epidemiologist David Fishman explained that the downward trend of cases in recent days is due to incomplete reporting. As a result, it may seem like the curve is flattening even though the number of cases continues to increase. See further explanation in Dr. Fishman's twitter thread: https://twitter.com/DFisman/status/1245692708376784896"),
    h5("The black dots represent reported Covid cases in Ontario. The red dots show the estimated number of cases, adjusted for the reporting delays. Use the slider to adjust the average reporting delay (in days)."),
    
    # Sidebar layout
    sidebarLayout(
        #Inputs: Select which inputs from the data we want to display
        sidebarPanel(
            #Select variable for y-axis
            sliderInput("slider1", h3("Average Delay in Reporting Deaths (in days)"),
                        min = 0, max = 15, value = 5)
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
        coord_cartesian(xlim= c(0,50), ylim= c(0,350))+ 
        
        geom_point(mapping = aes(x = days, y = case), color = "black")+
        
        geom_point(mapping = aes(x = days, y = case/(1-exp((-1/input$slider1)*(50-days)))), color = "red")
   
    })
}

# Run the application 
shinyApp(ui = ui, server = server)